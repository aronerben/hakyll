--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module Hakyll.Preview.Poll (
  watchUpdates,
) where

--------------------------------------------------------------------------------
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (
  newEmptyMVar,
  takeMVar,
  tryPutMVar,
 )
import Control.Exception (
  AsyncException,
  fromException,
  handle,
  throw,
 )
import Control.Monad (forever, void, when)
--------------------------------------------------------------------------------

import Data.List (isPrefixOf)
import Hakyll.Core.Configuration
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import System.Directory (canonicalizePath)
import qualified System.FSNotify as FSNotify
import System.FilePath (pathSeparators)

--------------------------------------------------------------------------------

-- | A thread that watches for updates in a 'providerDirectory' and recompiles
-- a site as soon as any changes occur
watchUpdates :: Configuration -> [FilePath] -> IO Pattern -> IO ()
watchUpdates conf excludedDirs update = do
  let providerDir = providerDirectory conf
  shouldBuild <- newEmptyMVar
  pattern <- update
  fullProviderDir <- canonicalizePath $ providerDirectory conf
  manager <- FSNotify.startManager

  let allowed event = do
        -- Absolute path of the changed file. This must be inside provider
        -- dir, since that's the only dir we're watching.
        let path = FSNotify.eventPath event
            relative =
              dropWhile (`elem` pathSeparators) $
                drop (length fullProviderDir) path
            identifier = fromFilePath relative
            isExcluded = any (`isPrefixOf` relative) excludedDirs

        shouldIgnore <- (|| isExcluded) <$> shouldIgnoreFile conf path
        return $ not shouldIgnore && matches pattern identifier

  -- This thread continually watches the `shouldBuild` MVar and builds
  -- whenever a value is present.
  _ <- forkIO $
    forever $ do
      event <- takeMVar shouldBuild
      handle
        ( \e -> case fromException e of
            Nothing -> putStrLn (show e)
            Just async -> throw (async :: AsyncException)
        )
        (update' event providerDir)

  -- Send an event whenever something occurs so that the thread described
  -- above will do a build.
  void $
    FSNotify.watchTree manager providerDir (not . isRemove) $ \event -> do
      allowed' <- allowed event
      when allowed' $ void $ tryPutMVar shouldBuild event
  where
    update' _ _ = void update

--------------------------------------------------------------------------------
isRemove :: FSNotify.Event -> Bool
isRemove (FSNotify.Removed {}) = True
isRemove _ = False

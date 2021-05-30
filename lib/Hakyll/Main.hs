--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

-- | Module providing the main hakyll function and command-line argument parsing
module Hakyll.Main (
  -- * Entry points
  hakyll,
  hakyllWith,
  hakyllWithArgs,
  hakyllWithExitCode,
  hakyllWithExitCodeAndArgs,

  -- * Command line argument parsers
  Options (..),
  Command (..),
  optionParser,
  commandParser,
  defaultParser,
  defaultParserPure,
  defaultParserPrefs,
  defaultParserInfo,
) where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

import qualified Hakyll.Check as Check
import qualified Hakyll.Commands as Commands
import qualified Hakyll.Core.Configuration as Config
import qualified Hakyll.Core.Logger as Logger
import Hakyll.Core.Rules
import Hakyll.Core.Util.Parser (directories)
import qualified Options.Applicative as OA
import System.Environment (getProgName)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.FilePath (isValid)
import System.IO.Unsafe (unsafePerformIO)
import qualified Text.Parsec as P
import Text.Printf (printf)

--------------------------------------------------------------------------------

-- | This usually is the function with which the user runs the hakyll compiler
hakyll :: Rules a -> IO ()
hakyll = hakyllWith Config.defaultConfiguration

--------------------------------------------------------------------------------

-- | A variant of 'hakyll' which allows the user to specify a custom
-- configuration
hakyllWith :: Config.Configuration -> Rules a -> IO ()
hakyllWith conf rules = hakyllWithExitCode conf rules >>= exitWith

--------------------------------------------------------------------------------

-- | A variant of 'hakyll' which returns an 'ExitCode'
hakyllWithExitCode :: Config.Configuration -> Rules a -> IO ExitCode
hakyllWithExitCode conf rules = do
  args <- defaultParser conf
  hakyllWithExitCodeAndArgs conf args rules

--------------------------------------------------------------------------------

-- | A variant of 'hakyll' which expects a 'Configuration' and command-line
-- 'Options'. This gives freedom to implement your own parsing.
hakyllWithArgs :: Config.Configuration -> Options -> Rules a -> IO ()
hakyllWithArgs conf args rules =
  hakyllWithExitCodeAndArgs conf args rules >>= exitWith

--------------------------------------------------------------------------------
hakyllWithExitCodeAndArgs ::
  Config.Configuration ->
  Options ->
  Rules a ->
  IO ExitCode
hakyllWithExitCodeAndArgs conf args rules = do
  let args' = optCommand args
      verbosity' = if verbosity args then Logger.Debug else Logger.Message
      check =
        if internal_links args' then Check.InternalLinks else Check.All

  logger <- Logger.new verbosity'
  invokeCommands args' conf check logger rules

--------------------------------------------------------------------------------
defaultParser :: Config.Configuration -> IO Options
defaultParser conf =
  OA.customExecParser defaultParserPrefs (defaultParserInfo conf)

--------------------------------------------------------------------------------
defaultParserPure :: Config.Configuration -> [String] -> OA.ParserResult Options
defaultParserPure conf =
  OA.execParserPure defaultParserPrefs (defaultParserInfo conf)

--------------------------------------------------------------------------------
defaultParserPrefs :: OA.ParserPrefs
defaultParserPrefs = OA.prefs OA.showHelpOnError

--------------------------------------------------------------------------------
defaultParserInfo :: Config.Configuration -> OA.ParserInfo Options
defaultParserInfo conf =
  OA.info
    (OA.helper <*> optionParser conf)
    ( OA.fullDesc
        <> OA.progDesc
          ( progName ++ " - Static site compiler created with Hakyll"
          )
    )

--------------------------------------------------------------------------------
invokeCommands ::
  Command ->
  Config.Configuration ->
  Check.Check ->
  Logger.Logger ->
  Rules a ->
  IO ExitCode
invokeCommands args conf check logger rules =
  case args of
    Build -> Commands.build conf logger rules
    Check _ -> Commands.check conf logger check
    Clean -> Commands.clean conf logger >> ok
    Deploy -> Commands.deploy conf
    Preview p -> Commands.preview conf logger rules p >> ok
    Rebuild -> Commands.rebuild conf logger rules
    Server _ _ -> Commands.server conf logger (host args) (port args) >> ok
    Watch _ p s e -> Commands.watch conf logger (host args) p (not s) e rules >> ok
  where
    ok = return ExitSuccess

--------------------------------------------------------------------------------

-- | The parsed command-line options.
data Options = Options {verbosity :: Bool, optCommand :: Command}
  deriving (Show)

-- | The command to run.
data Command
  = -- | Generate the site.
    Build
  | -- | Validate the site output.
    Check {internal_links :: Bool}
  | -- | Clean up and remove cache.
    Clean
  | -- | Upload/deploy your site.
    Deploy
  | -- | [DEPRECATED] Please use the watch command.
    Preview {port :: Int}
  | -- | Clean and build again.
    Rebuild
  | -- | Start a preview server.
    Server {host :: String, port :: Int}
  | -- | Autocompile on changes and start a preview server.
    Watch {host :: String, port :: Int, no_server :: Bool, exclude_dir :: [FilePath]}
  deriving (Show)

{-# DEPRECATED Preview "Use Watch instead." #-}

optionParser :: Config.Configuration -> OA.Parser Options
optionParser conf = Options <$> verboseParser <*> commandParser conf
  where
    verboseParser = OA.switch (OA.long "verbose" <> OA.short 'v' <> OA.help "Run in verbose mode")

directoriesParser :: OA.ReadM [FilePath]
directoriesParser =
  OA.eitherReader
    ( \str ->
        case P.parse directories "" str of
          Left _ -> Left $ printf "cannot parse value '%s'" str
          Right dirs ->
            mapM
              ( \dir ->
                  if isValid dir
                    then Right dir
                    else Left $ printf "invalid dir '%s'" dir
              )
              dirs
    )

commandParser :: Config.Configuration -> OA.Parser Command
commandParser conf = OA.subparser $ foldr ((<>) . produceCommand) mempty commands
  where
    portParser = OA.option OA.auto (OA.long "port" <> OA.help "Port to listen on" <> OA.value (Config.previewPort conf))
    hostParser = OA.strOption (OA.long "host" <> OA.help "Host to bind on" <> OA.value (Config.previewHost conf))

    produceCommand (c, a, b) = OA.command c (OA.info (OA.helper <*> a) b)

    commands =
      [
        ( "build"
        , pure Build
        , OA.fullDesc <> OA.progDesc "Generate the site"
        )
      ,
        ( "check"
        , Check <$> OA.switch (OA.long "internal-links" <> OA.help "Check internal links only")
        , OA.fullDesc <> OA.progDesc "Validate the site output"
        )
      ,
        ( "clean"
        , pure Clean
        , OA.fullDesc <> OA.progDesc "Clean up and remove cache"
        )
      ,
        ( "deploy"
        , pure Deploy
        , OA.fullDesc <> OA.progDesc "Upload/deploy your site"
        )
      ,
        ( "preview"
        , Preview <$> portParser
        , OA.fullDesc <> OA.progDesc "[DEPRECATED] Please use the watch command"
        )
      ,
        ( "rebuild"
        , pure Rebuild
        , OA.fullDesc <> OA.progDesc "Clean and build again"
        )
      ,
        ( "server"
        , Server <$> hostParser <*> portParser
        , OA.fullDesc <> OA.progDesc "Start a preview server"
        )
      ,
        ( "watch"
        , -- TODO [aerben] add tests and docs
          Watch <$> hostParser <*> portParser <*> OA.switch (OA.long "no-server" <> OA.help "Disable the built-in web server") <*> OA.option directoriesParser (OA.long "exclude-dir" <> OA.help "Exclude one or more directories from being watched" <> OA.value [])
        , OA.fullDesc <> OA.progDesc "Autocompile on changes and start a preview server. You can watch and recompile without running a server with --no-server. You can exclude one or more directories with --exclude-dir=\"dir\", --exclude-dir=\"{dir1,dir2}\"."
        )
      ]

--------------------------------------------------------------------------------

-- | This is necessary because not everyone calls their program the same...
progName :: String
progName = unsafePerformIO getProgName
{-# NOINLINE progName #-}

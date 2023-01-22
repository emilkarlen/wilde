-- | Parse CLI arguments and read config files,
-- to resolve all configuration available via CLI.

{-# LANGUAGE StrictData #-}

module Wilde.Driver.Application.WaiServer.Cli.Read
(
    FileHandlingConfiguration(..),
    Logging(..),
    ServerConfiguration(..),
    Configuration(..),
    eliminate,

    read,
    exec,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Prelude hiding (read)

import Data.Maybe (catMaybes)

import qualified System.IO as SysIO

import qualified Options.Applicative as OptParse

import Wilde.Driver.Application.WaiServer.RequestHandling.Main.Types (FileHandlingConfiguration(..))

import qualified Wilde.Driver.Application.WaiServer.Cli.Parse as Parse
import qualified Wilde.Driver.Application.WaiServer.ConfigFile.Read as ReadConfFile
import qualified Wilde.Driver.Application.WaiServer.ConfigFile.Configuration as ConfFileConf
import qualified System.Exit as SysExit
import qualified Wilde.Utils.Logging.Entry as LoggingEntry


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Output from reading config via CLI.
data Configuration
    = Server ServerConfiguration
    | PrintDefaults
    | ConfigurationHelp
    deriving (Eq, Show)

eliminate :: a -- ^ print defaults
          -> a -- ^ configuration help
          -> (ServerConfiguration -> a)
          -> Configuration -> a
eliminate pd _ _   PrintDefaults = pd
eliminate _ ch _   ConfigurationHelp = ch
eliminate _ _ srvr (Server configuration) = srvr configuration

data ServerConfiguration = ServerConfiguration
    {
        port          :: Int
    ,   fileHandling  :: FileHandlingConfiguration
    ,   logging       :: Logging
    }
    deriving (Eq, Show)

data Logging = Logging
    {
        logging_enabled :: Bool
    ,   logging_level   :: LoggingEntry.Level
    }
    deriving (Eq, Show)

-- | Parses CLI arguments and merges
-- configuration from
--   - defaults
--   - CLI arguments
--   - config file, references via CLI arguments
--
-- On argument syntax error, or help request,
-- this method exits the current process.
exec :: String
        -- ^ Application name, to present in help.
        -- See help output for details.
     -> ServerConfiguration
     -> IO Configuration
exec appName defaults = do
    arguments <- Parse.exec appName
    handle defaults id exitWithErrMsg arguments
    where
        exitWithErrMsg :: String -> IO a
        exitWithErrMsg errMsg = do
            SysIO.hPutStrLn SysIO.stderr errMsg
            SysExit.exitFailure

-- Same as `exec` but pures an error message
-- instead of exiting.
--
-- Exposed for testing purposes.
--
-- Help requests are not handled, but reported as
-- an error instead.
read :: String
        -- ^ Application name, to present in help.
        -- See help output for details.
     -> ServerConfiguration
        -- ^ defaults
     -> [String]
        -- ^ CLI arguments
     -> IO (Either String Configuration)
        -- ^ Either an error message or
        -- a successfully read CLI configuration.
read appName defaults cli_arguments =
    case cli_parse_result of
        OptParse.Success arguments   -> handle defaults Right errorHandler arguments
        OptParse.Failure _           -> pure $ Left "Failure"
        OptParse.CompletionInvoked _ -> pure $ Left "CompletionInvoked"
    where
        errorHandler :: String -> IO (Either String atConfiguration)
        errorHandler errMsg = pure $ Left errMsg

        cli_parse_result = Parse.parseArgs appName cli_arguments

readConfigAndMerge
    :: ServerConfiguration
       -- ^ defaults
    -> Parse.Arguments
       -- CLI arguments
    -> IO (Either String ServerConfiguration)
readConfigAndMerge defaults arguments = fmap merge <$> configFromFile
    where
        configFromFile :: IO (Either String ReadConfFile.Configuration)
        configFromFile =
            case Parse.confFile arguments of
                Nothing       -> pure $ Right ConfFileConf.empty
                Just filePath -> ReadConfFile.read filePath

        merge :: ReadConfFile.Configuration -> ServerConfiguration
        merge (ReadConfFile.Configuration mbPort mbFiles mbLogging) = ServerConfiguration
            {
                port         = resolve (port defaults) [mbPort, Parse.port arguments]
            ,   fileHandling = FileHandlingConfiguration
                    {
                        filePaths         = resolve (filePaths        dfltFileHandling) [mbFiles >>= ReadConfFile.file_paths]
                    ,   handledMimeTypes  = resolve (handledMimeTypes dfltFileHandling) [mbFiles >>= ReadConfFile.mime_types]
                    }
            ,   logging = Logging
                    {
                        logging_enabled = resolve (logging_enabled dfltLogging) [mbLogging >>= ReadConfFile.enabled, Parse.loggingEnabled arguments]
                    ,   logging_level   = resolve (logging_level   dfltLogging) [mbLogging >>= ReadConfFile.level  , Parse.loggingLevel   arguments]
                    }
            }
            where
                dfltLogging :: Logging
                dfltLogging = logging defaults

                dfltFileHandling :: FileHandlingConfiguration
                dfltFileHandling = fileHandling defaults

        resolve :: a -> [Maybe a] -> a
        resolve the_default mbs = last $ the_default : catMaybes mbs

-- | Utility for parsing CLI arguments and handling the result.
--
-- Shared behaviour of `exec` and `read`
handle :: ServerConfiguration -> (Configuration -> a) -> (String -> IO a) -> Parse.Arguments -> IO a
handle defaults successOf errorHandler cliArguments
  | Parse.printDefaults cliArguments = pure $ successOf PrintDefaults
  | Parse.printConfHelp cliArguments = pure $ successOf ConfigurationHelp
  | otherwise = do
    errMsgOrServerConf <- readConfigAndMerge defaults cliArguments
    case errMsgOrServerConf of
        Left errMsg      -> errorHandler errMsg
        Right serverConf -> pure $ successOf $ Server serverConf

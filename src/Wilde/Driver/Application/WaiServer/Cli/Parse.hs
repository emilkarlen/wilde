module Wilde.Driver.Application.WaiServer.Cli.Parse
    (
        exec,
        parseArgs,

        Arguments(..),

        -- * options

        module Opts,
    )
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Options.Applicative hiding (info)
import qualified Options.Applicative as OptParse

import           Wilde.Driver.Application.WaiServer.Cli.Arguments (Arguments(..))
import qualified Wilde.Driver.Application.WaiServer.Cli.Help as Help
import           Wilde.Driver.Application.WaiServer.Cli.Options as Opts

import qualified Wilde.Utils.Logging.Entry as Logging


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


parseArgs :: String
             -- ^ Application name, to present in help.
             -- See help output for details.
         -> [String] -> OptParse.ParserResult Arguments
parseArgs appName = OptParse.execParserPure preferences (info appName)

-- | Parses CLI arguments and exits on failure or
-- help request.
exec :: String
        -- ^ Application name, to present in help.
        -- See help output for details.
     -> IO Arguments
exec appName = OptParse.execParser (info appName)

parser :: Parser Arguments
parser =
    Arguments
    <$> parser_port
    <*> parser_logging_enabled
    <*> parser_logging_level
    <*> parser_config_file
    <*> parser_print_defaults
    <*> parser_conf_help
    <**> helper

preferences :: ParserPrefs
preferences = prefs showHelpOnError

info :: String
        -- ^ Application name, to present in help.
        -- See help output for details.
     -> ParserInfo Arguments
info appName = OptParse.info parser $
    fullDesc
    <> headerDoc   (Just $ Help.header appName)
    <> footerDoc   (Just Help.footer)
    <> failureCode 1
    <> noIntersperse


-------------------------------------------------------------------------------
-- - parsers for options -
-------------------------------------------------------------------------------


parser_port :: Parser (Maybe Int)
parser_port = optional $ option auto
    (  long    Opts.port_long
    <> short   Opts.port_short
    <> metavar "N"
    <> help  "Port for the server to listen on"
    )

parser_logging_enabled :: Parser (Maybe Bool)
parser_logging_enabled = optional $ option (eitherReader Opts.read_bool)
    (  long    Opts.logging_enabled_long
    <> metavar bool_val_meta_var
    <> help  "Turn logging on of off"
    )

parser_logging_level :: Parser (Maybe Logging.Level)
parser_logging_level = optional $ option auto
    (  long    Opts.logging_level_long
    <> metavar "LEVEL"
    <> help    "Set the logging level"
    )

parser_config_file :: Parser (Maybe FilePath)
parser_config_file = optional $ strOption
    (  long    Opts.conf_file_long
    <> short   Opts.conf_file_short
    <> metavar "PATH"
    <> help  "Read configuration from a file"
    )

parser_print_defaults :: Parser Bool
parser_print_defaults = switch
    (  long  Opts.print_defaults_long
    <> help  "Print the built-in default configuration (formatted as a configuration file), and exit"
    )

parser_conf_help :: Parser Bool
parser_conf_help = switch
    (  long  Opts.print_conf_help_long
    <> help  "Print help on configuration, and exit"
    )

bool_val_meta_var :: String
bool_val_meta_var =
    "(" <>
    Opts.value_false_std <>
    "|" <>
    Opts.value_true_std <>
    ")"
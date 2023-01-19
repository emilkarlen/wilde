module WildeTest.Driver.WaiServer.Cli.TestResources.CliOptions where

import Wilde.Utils.Logging.Entry (Level)

import qualified Wilde.Driver.Application.WaiServer.Cli.Options as Options
import qualified Wilde.Driver.Application.WaiServer.Cli.Parse as Options

short :: Char -> String
short x = "-" <> [x]

long :: String -> String
long x = "--" <> x

bool_val :: Bool -> String
bool_val False = Options.value_false_std
bool_val True  = Options.value_true_std

logging_level_val :: Level -> String
logging_level_val x = show x

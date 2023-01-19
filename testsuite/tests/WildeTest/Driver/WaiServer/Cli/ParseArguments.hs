module WildeTest.Driver.WaiServer.Cli.ParseArguments
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.List
import qualified Data.Text as T
import qualified Data.Map as M

import           System.Exit (ExitCode(..))

import qualified Options.Applicative as OptParse


import           Test.HUnit
import           Test.HUnit.Lang (FailureReason(..))

import qualified Wilde.Driver.Application.WaiServer.Cli.Parse as Sut
import qualified Wilde.Driver.Application.WaiServer.Cli.Arguments as Settings

import qualified Wilde.Utils.Logging.Entry as Logging

import qualified WildeTest.Driver.WaiServer.Cli.TestResources.CliOptions as CliOptions
import           WildeTest.Driver.WaiServer.Cli.TestResources

import           TestResources.Testing.Checker as Checker (Checker, test, PropertyGetter, property, eq, ne)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theTest :: Test
theTest =
  TestLabel "parse" $
  TestList
    [
        "no args - default arguments" ~:
        isSuccess default_arguments $ Sut.parseArgs applicationName []

    ,   "port" ~:
        port

    ,   "config-file" ~:
        config_file

    ,   "logging" ~:
        logging

    ,   "dump-defaults" ~:
        print_defaults

    ,   "help" ~:
        help
    ]

port :: Test
port =
    TestList
    [
        "success" ~:
        shortAndLong_port ["5"] $ isSuccess (default_arguments { Settings.port = Just 5 } )

    ,   "failure - not an int" ~:
        shortAndLong_port ["x"] $ isFailureWithExitCodeForNotSuccess

    ,   "failure - superfluous positional arg" ~:
        shortAndLong_port ["5", "superfluous"] $ isFailureWithExitCodeForNotSuccess

    ,   "failure - missing argument" ~:
        shortAndLong_port [] $ isFailureWithExitCodeForNotSuccess
    ]
    where
        shortAndLong_port = shortAndLong Sut.port_short Sut.port_long

config_file :: Test
config_file =
    TestList
    [
        "success" ~:
        shortAndLong_confFile ["the-file/path"] $ isSuccess (default_arguments { Settings.confFile = Just "the-file/path" } )

    ,   "failure - superfluous positional arg" ~:
        shortAndLong_confFile ["file/path", "superfluous"] $ isFailureWithExitCodeForNotSuccess

    ,   "failure - missing argument" ~:
        shortAndLong_confFile [] $ isFailureWithExitCodeForNotSuccess
    ]
    where
        shortAndLong_confFile = shortAndLong Sut.conf_file_short Sut.conf_file_long

logging :: Test
logging =
    TestList
    [
        "enabled" ~: logging_enabled
    ,   "level"   ~: logging_level
    ]

logging_enabled :: Test
logging_enabled =
    TestList
    [
        "success" ~:
        TestList
        [
            show expected ~:
            test [CliOptions.bool_val expected] $
            isSuccess (default_arguments { Settings.loggingEnabled = Just expected } )
            | expected <- [False, True]
        ]

    ,   "failure" ~:
        TestList
        [
            "missing argument" ~:
            test [] $
            isFailureWithExitCodeForNotSuccess

        ,   "invalid argument" ~:
            test ["not_a_bool"] $
            isFailureWithExitCodeForNotSuccess
        ]
    ]
    where
        test = onlyLong Sut.logging_enabled_long

logging_level :: Test
logging_level =
    TestList
    [
        "success" ~:
        TestList
        [
            show expected ~:
            test [CliOptions.logging_level_val expected] $
            isSuccess (default_arguments { Settings.loggingLevel = Just expected } )
            | expected <- enumFrom minBound
        ]

    ,   "failure" ~:
        TestList
        [
            "missing argument" ~:
            test [] $
            isFailureWithExitCodeForNotSuccess

        ,   "invalid argument" ~:
            test ["not_a_level"] $
            isFailureWithExitCodeForNotSuccess
        ]
    ]
    where
        test = onlyLong Sut.logging_level_long

print_defaults :: Test
print_defaults =
    TestList
    [
        "success" ~:
        test [] $ isSuccess (default_arguments { Settings.printDefaults = True } )

    ,   "failure - superfluous positional arg" ~:
        test [CliOptions.long "superfluous"] isFailureWithExitCodeForNotSuccess
    ]
    where
        test = onlyLong Sut.print_defaults_long


help :: Test
help =
    TestList
    [
        "success" ~:
        shortAndLong_help [] isFailureWithExitCodeForSuccess

    ,   "failure - superfluous positional arg" ~:
        shortAndLong_help ["superfluous"] isFailureWithExitCodeForSuccess
    ]
    where
        shortAndLong_help = shortAndLong 'h' "help"


-------------------------------------------------------------------------------
-- - utils -
-------------------------------------------------------------------------------


isFailureWithExitCodeForSuccess :: OptParse.ParserResult Settings.Arguments -> Test
isFailureWithExitCodeForSuccess =
    Checker.test $ checkIsFailure2 $ property exit_code (eq ExitSuccess)

isFailureWith :: OptParse.ParserResult Settings.Arguments -> Test
isFailureWith =
    Checker.test $ checkIsFailure2 $ property exit_code (eq ExitSuccess)

isFailureWithExitCodeForNotSuccess :: OptParse.ParserResult Settings.Arguments -> Test
isFailureWithExitCodeForNotSuccess =
    Checker.test $ checkIsFailure2 $ property exit_code isExitFail

exit_code :: PropertyGetter (OptParse.ParserHelp, ExitCode, Int) ExitCode
exit_code = ("exit code", \(a,b,c) -> b)

isExitFail :: Checker.Checker ExitCode
isExitFail super_components ExitSuccess = Just (super_components, ExpectedButGot Nothing "ExitFailure" (show ExitSuccess))
isExitFail super_components _ = Nothing

default_arguments =
    Sut.Arguments
    {
        Sut.port           = Nothing
    ,   Sut.confFile       = Nothing
    ,   Sut.loggingEnabled = Nothing
    ,   Sut.loggingLevel   = Nothing
    ,   Sut.printDefaults  = False
    ,   Sut.printConfHelp  = False
    }

applicationName :: String
applicationName = "Test Application Name"

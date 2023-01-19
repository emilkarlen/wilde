{-# LANGUAGE OverloadedStrings #-}
module WildeTest.Driver.WaiServer.Cli.ReadArgsAndFiles
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.String as S
import           Data.List
import qualified Data.Text as T
import qualified Data.Map as M
import           Data.ByteString as B

import           Control.Exception (AssertionFailed(AssertionFailed))

import qualified Options.Applicative as OptParse

import qualified Data.Yaml as Yaml
import qualified TestResources.Testing.TmpDir as TmpDir
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK

import qualified Data.Text.Encoding as TextEncoding

import qualified Wilde.Driver.Application.WaiServer.Cli.Read as Sut
import qualified Wilde.Driver.Application.WaiServer.Cli.Parse as Parse
import qualified Wilde.Driver.Application.WaiServer.Cli.Options as Opts
import qualified Wilde.Driver.Application.WaiServer.Cli.Arguments as Settings
import qualified Wilde.Driver.Application.WaiServer.ConfigFile.Configuration as ConfFileConf

import qualified WildeTest.Driver.WaiServer.Cli.TestResources as TR
import qualified WildeTest.Driver.WaiServer.Cli.TestResources.Yaml as YTR
import qualified WildeTest.Driver.WaiServer.Cli.TestResources.CliOptions as CliOptions
import           Wilde.Driver.Application.WaiServer.ConfigFile.Yaml

import qualified Wilde.Utils.Logging.Entry as Logging

import           Test.HUnit hiding (failures)

import qualified TestResources.Testing.TestWithTmpDir as TestWithTmpDir
import           TestResources.Testing.Checker as Checker


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theTest :: Test
theTest =
    TestLabel "read" $
    TestList
    [
        "successes" ~: successes
    ,   "failures"  ~: failures
    ]

successes :: Test
successes =
    TestList
    [
        "WHEN no arguments, defaults SHOULD be used" ~:
        when_no_arguments_defaults_should_be_used

    ,   "WHEN config file given, values from it SHOULD override defaults" ~:
        config_file_overrides_defaults

    ,   "resolving order SHOULD be cli-args - config-file - defaults" ~:
        cli_args_override_config_file
    ]

failures :: Test
failures =
    TestList
    [
        "fail WHEN invalid arguments" ~:
        testCase a_config ["invalid-argument"] isFailure

    ,   "fail WHEN config file does not exist" ~:
        testCase a_config [CliOptions.short Parse.conf_file_short, "/non existing file"] isFailure

    ,   "fail WHEN syntax of config file is invalid" ~:
        invalid_syntax_of_config_file
    ]

isServerConfig :: Sut.ServerConfiguration -> Checker (Either String Sut.Configuration)
isServerConfig config = isRightOf $ eq $ Sut.Server config

when_no_arguments_defaults_should_be_used :: Test
when_no_arguments_defaults_should_be_used =
    TestList
    [
        show lg ~:
        check lg_enabled lg_level |
        lg@(lg_enabled, lg_level) <- logging_cases
    ]
    where
        logging_cases :: [(Bool, Logging.Level)]
        logging_cases = [(False, Logging.DEBUG), (True, Logging.INFO)]

        check :: Bool -> Logging.Level -> Test
        check logging_enabled logging_level =
            let
                config = config_of logging_enabled logging_level
            in
                testCase config [] (isServerConfig config)

        config_of :: Bool -> Logging.Level -> Sut.ServerConfiguration
        config_of logging_enabled logging_level = Sut.ServerConfiguration
            {
                Sut.port = 5
            ,   Sut.fileHandling = Sut.FileHandlingConfiguration
                    {
                        Sut.filePaths         = M.singleton ["default-path-comp"] "default fs-path"
                    ,   Sut.handledMimeTypes  = M.singleton "default-ext" "default mime type"
                    }
            ,   Sut.logging = Sut.Logging
                    {
                        Sut.logging_enabled = logging_enabled
                    ,   Sut.logging_level   = logging_level
                    }
            }

invalid_syntax_of_config_file :: Test
invalid_syntax_of_config_file =
    testWTmpFile_b a_config "config.yaml" (S.fromString "this is not valid\n") getCliArguments isFailure
    where
        getCliArguments :: FilePath -> [String]
        getCliArguments tmpFilePath = [CliOptions.long Parse.conf_file_long, tmpFilePath]


config_file_overrides_defaults :: Test
config_file_overrides_defaults =
    testWTmpFile defaults "config.yaml" confFile getCliArguments (isServerConfig expected)
    where
        getCliArguments :: FilePath -> [String]
        getCliArguments tmpFilePath = [CliOptions.long Parse.conf_file_long, tmpFilePath]

        defaults :: Sut.ServerConfiguration
        defaults = Sut.ServerConfiguration
            {
                Sut.port = 5
            ,   Sut.fileHandling = Sut.FileHandlingConfiguration
                    {
                        Sut.filePaths         = M.singleton ["comp1"] "default fs-path"
                    ,   Sut.handledMimeTypes  = M.singleton "default-ext" "default mime type"
                    }
            ,   Sut.logging = Sut.Logging
                    {
                        Sut.logging_enabled = False
                    ,   Sut.logging_level   = Logging.DEBUG
                    }
            }

        expected :: Sut.ServerConfiguration
        expected = Sut.ServerConfiguration
            {
                Sut.port         = 10
            ,   Sut.fileHandling = expected_fh
            ,   Sut.logging      = expected_lg
            }

        expected_fh :: Sut.FileHandlingConfiguration
        expected_fh = Sut.FileHandlingConfiguration
            {
                Sut.filePaths         = M.singleton ["comp1", "comp2"] "conf-fs-path"
            ,   Sut.handledMimeTypes  = M.singleton "conf-ext" "conf mime type"
            }

        expected_lg :: Sut.Logging
        expected_lg = Sut.Logging
            {
                Sut.logging_enabled  = not  $ Sut.logging_enabled $ Sut.logging defaults
            ,   Sut.logging_level    = succ $ Sut.logging_level   $ Sut.logging defaults
            }

        confFile :: ConfFileConf.Configuration
        confFile = ConfFileConf.Configuration
            {
                ConfFileConf.port          = Just $ Sut.port expected
            ,   ConfFileConf.files         = Just
                    ConfFileConf.Files
                    {
                        ConfFileConf.file_paths    = Just $ Sut.filePaths        expected_fh
                    ,   ConfFileConf.mime_types    = Just $ Sut.handledMimeTypes expected_fh
                    }
            ,   ConfFileConf.logging       = Just
                    ConfFileConf.Logging
                    {
                        ConfFileConf.enabled = Just $ Sut.logging_enabled expected_lg
                    ,   ConfFileConf.level   = Just $ Sut.logging_level   expected_lg
                    }
            }

cli_args_override_config_file :: Test
cli_args_override_config_file =
    testWTmpFile defaults "config.yaml" confFile getCliArguments (isServerConfig expected)
    where
        getCliArguments :: FilePath -> [String]
        getCliArguments tmpFilePath =
            [ CliOptions.long Parse.conf_file_long, tmpFilePath
            , CliOptions.long Parse.port_long, show cli_arg_port
            , CliOptions.long Parse.logging_enabled_long, CliOptions.bool_val expected_lg_enabled
            , CliOptions.long Parse.logging_level_long, CliOptions.logging_level_val expected_lg_level
            ]

        expected_lg_enabled :: Bool
        expected_lg_enabled = True

        expected_lg_level :: Logging.Level
        expected_lg_level = Logging.INFO

        cli_arg_port :: Int
        cli_arg_port = 72

        defaults :: Sut.ServerConfiguration
        defaults = Sut.ServerConfiguration
            {
                Sut.port = cli_arg_port + 1
            ,   Sut.fileHandling = Sut.FileHandlingConfiguration
                    {
                        Sut.filePaths         = M.singleton ["comp1"] "default fs-path"
                    ,   Sut.handledMimeTypes  = M.singleton "default-ext" "default mime type"
                    }
            ,   Sut.logging =
                    Sut.Logging
                    {
                        Sut.logging_enabled = not expected_lg_enabled
                    ,   Sut.logging_level   = succ expected_lg_level
                    }
            }

        expected :: Sut.ServerConfiguration
        expected = Sut.ServerConfiguration
            {
                Sut.port = cli_arg_port
            ,   Sut.fileHandling = Sut.FileHandlingConfiguration
                    {
                        Sut.filePaths         = M.singleton ["comp1", "comp2"] "conf-fs-path"
                    ,   Sut.handledMimeTypes  = Sut.handledMimeTypes $ Sut.fileHandling defaults
                    }
            ,   Sut.logging = Sut.Logging
                    {
                        Sut.logging_enabled = expected_lg_enabled
                    ,   Sut.logging_level   = expected_lg_level
                    }
            }

        confFile :: ConfFileConf.Configuration
        confFile = ConfFileConf.Configuration
            {
                ConfFileConf.port          = Just $ cli_arg_port + 2
            ,   ConfFileConf.files         = Just
                    ConfFileConf.Files
                    {
                        ConfFileConf.file_paths    = Just $ Sut.filePaths $ Sut.fileHandling expected
                    ,   ConfFileConf.mime_types    = Nothing
                    }
            ,   ConfFileConf.logging = Just
                    ConfFileConf.Logging
                    {
                        ConfFileConf.enabled = Just $ not expected_lg_enabled
                    ,   ConfFileConf.level   = Just $ succ expected_lg_level
                    }
            }

a_config :: Sut.ServerConfiguration
a_config = Sut.ServerConfiguration
    {
        Sut.port = 5
    ,   Sut.fileHandling = Sut.FileHandlingConfiguration
            {
                Sut.filePaths         = M.singleton ["default-path-comp"] "default fs-path"
            ,   Sut.handledMimeTypes  = M.singleton "default-ext" "default mime type"
            }
    ,   Sut.logging = Sut.Logging
            {
                Sut.logging_enabled = False
            ,   Sut.logging_level   = Logging.WARNING
            }
    }

testWTmpFile
    :: Sut.ServerConfiguration    -- ^ defaults
    -> String                     -- ^ file base name
    -> ConfFileConf.Configuration -- ^ file contents
    -> (FilePath -> [String])     -- ^ arguments, given path of tmp file
    -> Checker (Either String Sut.Configuration)
    -> Test
testWTmpFile defaults fileName fileContents getCliArgs check =
    testWTmpFile_b defaults fileName fileContents_b getCliArgs check
    where
        fileContents_b :: ByteString
        fileContents_b = Yaml.encode $ Yaml.toJSON fileContents

testWTmpFile_b
    :: Sut.ServerConfiguration -- ^ defaults
    -> String                  -- ^ file base name
    -> ByteString              -- ^ file contents
    -> (FilePath -> [String])  -- ^ arguments, given path of tmp file
    -> Checker (Either String Sut.Configuration)
    -> Test
testWTmpFile_b defaults fileName fileContents getCliArgs check =
    TestCase $ TestWithTmpDir.assertWithTmpFile_b fileName fileContents assertion
    where
        assertion :: FilePath -> Assertion
        assertion filePath = do
            actual <- Sut.read applicationName defaults (getCliArgs filePath)
            Checker.assert check actual

testCase
    :: Sut.ServerConfiguration
       -- ^ defaults
    -> [String]
       -- CLI arguments
    -> Checker (Either String Sut.Configuration)
    -> Test
testCase defaults cliArguments check = TestCase assertion
    where
        assertion :: Assertion
        assertion = do
            actual <- Sut.read applicationName defaults cliArguments
            Checker.assert check actual

isFailure :: Checker (Either String Sut.Configuration)
isFailure = Checker.isLeftOf Checker.anything

applicationName :: String
applicationName = "Test Application Name"

-- | Utilities for constructing assertions.
{-# LANGUAGE ScopedTypeVariables #-}
module WildeTest.Driver.WaiServer.Cli.TestResources
  (
        -- * Assertions

        isSuccess,
        isFailure,

        asrtIsSuccess,
        asrtIsFailure,

        checkIsFailure,
        checkIsFailure2,

        -- * test helpers

        shortAndLong,
        onlyLong,
  )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           System.Exit (ExitCode)

import qualified Options.Applicative as OptParse

import           Test.HUnit.Base
import           Test.HUnit.Lang (FailureReason(..), formatFailureReason)



import           Wilde.Driver.Application.WaiServer.Cli.Arguments
import qualified Wilde.Driver.Application.WaiServer.Cli.Parse as Parse
import qualified Wilde.Driver.Application.WaiServer.ConfigFile.Read as ParseConfigFile

import           TestResources.Testing.Checker

import           TestResources.Testing.AssertUtils
import qualified TestResources.Testing.TmpDir as TmpDir
import qualified WildeTest.Driver.WaiServer.Cli.TestResources.CliOptions as CliOptions


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - assertions -
-------------------------------------------------------------------------------


-- | Test equivalent result of short and long option alternatives.
shortAndLong :: Char -> String -> [String] -> (OptParse.ParserResult Arguments -> Test) -> Test
shortAndLong short long args test =
  TestList
  [
    "short option" ~:
    test $ Parse.parseArgs applicationName (CliOptions.short short : args)

  , "long option" ~:
    test $ Parse.parseArgs applicationName (CliOptions.long long : args)
  ]

-- | Test just a long option.
onlyLong :: String -> [String] -> (OptParse.ParserResult Arguments -> Test) -> Test
onlyLong long args test =
    test $ Parse.parseArgs applicationName (CliOptions.long long : args)


isFailure :: Show a => OptParse.ParserResult a -> Test
isFailure = TestCase . asrtIsFailure

isSuccess :: Arguments -> OptParse.ParserResult Arguments -> Test
isSuccess expected actual = TestCase $ asrtIsSuccess expected actual

asrtIsFailure :: Show a => OptParse.ParserResult a -> Assertion
asrtIsFailure (OptParse.Success x)           = failExpectedButGot "Failure <anything>" ("Success " <> show x)
asrtIsFailure (OptParse.CompletionInvoked x) = failExpectedButGot "Failure <anything>" "CompletionInvoked"
asrtIsFailure _                              = pure ()

asrtIsSuccess :: Arguments -> OptParse.ParserResult Arguments -> Assertion
asrtIsSuccess expected (OptParse.Success actual)      = assertEqual "" expected actual
asrtIsSuccess expected (OptParse.CompletionInvoked x) = failExpectedButGot "Success" "CompletionInvoked"
asrtIsSuccess expected (OptParse.Failure x)           = failExpectedButGot "Success" ("Failure " <> show x)

checkIsFailure :: forall a h. Show a => Checker (OptParse.ParserFailure OptParse.ParserHelp) -> Checker (OptParse.ParserResult a)
checkIsFailure failure_checker super_components = ret_val
  where
    ret_val :: OptParse.ParserResult a -> Maybe ([String], FailureReason)
    ret_val (OptParse.Failure x)           = failure_checker ("Failure" : super_components) x
    ret_val (OptParse.Success x)           = Just $ (super_components, ExpectedButGot (Just "vad är det här??") "Failure" ("Success " <> show x))
    ret_val (OptParse.CompletionInvoked x) = Just $ (super_components, ExpectedButGot (Just "vad är det här??") "Failure" "CompletionInvoked")

checkIsFailure2 :: Show a => Checker (OptParse.ParserHelp, ExitCode, Int) -> Checker (OptParse.ParserResult a)
checkIsFailure2 failure_checker super_components = checkIsFailure failure_checker' super_components
  where
    failure_checker' :: Checker (OptParse.ParserFailure OptParse.ParserHelp)
    failure_checker' super_components (OptParse.ParserFailure execFailure) =
      failure_checker super_components $ execFailure ""

applicationName :: String
applicationName = "Application Name"

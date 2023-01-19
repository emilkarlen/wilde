module WildeTest.Driver.WaiServer.Cli.Tests
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import qualified WildeTest.Driver.WaiServer.Cli.Options
import qualified WildeTest.Driver.WaiServer.Cli.ParseArguments
import qualified WildeTest.Driver.WaiServer.Cli.ReadArgsAndFiles


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theTest :: Test
theTest = TestList
          [
            "options" ~:
            WildeTest.Driver.WaiServer.Cli.Options.theTest
          , "parse arguments" ~:
            WildeTest.Driver.WaiServer.Cli.ParseArguments.theTest
          , "read args and files" ~:
            WildeTest.Driver.WaiServer.Cli.ReadArgsAndFiles.theTest
          ]

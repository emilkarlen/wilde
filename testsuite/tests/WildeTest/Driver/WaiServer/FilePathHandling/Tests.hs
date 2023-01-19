module WildeTest.Driver.WaiServer.FilePathHandling.Tests
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import qualified WildeTest.Driver.WaiServer.FilePathHandling.PathIsValid
import qualified WildeTest.Driver.WaiServer.FilePathHandling.ResolveMimeType
import qualified WildeTest.Driver.WaiServer.Cli.Tests


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theTest :: Test
theTest = TestList
          [ "path is valid" ~:
            WildeTest.Driver.WaiServer.FilePathHandling.PathIsValid.theTest
          , "resolve mime type" ~:
            WildeTest.Driver.WaiServer.FilePathHandling.ResolveMimeType.theTest
          , "cli" ~:
            WildeTest.Driver.WaiServer.Cli.Tests.theTest
          ]

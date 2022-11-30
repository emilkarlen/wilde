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


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theTest :: Test
theTest = TestList
          [ WildeTest.Driver.WaiServer.FilePathHandling.PathIsValid.theTest
          , WildeTest.Driver.WaiServer.FilePathHandling.ResolveMimeType.theTest
          ]

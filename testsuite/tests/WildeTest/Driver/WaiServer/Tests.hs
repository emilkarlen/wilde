module WildeTest.Driver.WaiServer.Tests
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import qualified WildeTest.Driver.WaiServer.RequestHandlerResolving
import qualified WildeTest.Driver.WaiServer.FilePathHandling.Tests
import qualified WildeTest.Driver.WaiServer.ReadConfigFile


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theTest :: Test
theTest = TestList
          [ WildeTest.Driver.WaiServer.RequestHandlerResolving.theTest
          , WildeTest.Driver.WaiServer.FilePathHandling.Tests.theTest
          , WildeTest.Driver.WaiServer.ReadConfigFile.theTest
          ]

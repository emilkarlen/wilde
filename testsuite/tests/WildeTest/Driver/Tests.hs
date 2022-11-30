module WildeTest.Driver.Tests
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import qualified WildeTest.Driver.WaiServer.Tests


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | All tests in this package.
theTest :: Test
theTest = TestList
          [ WildeTest.Driver.WaiServer.Tests.theTest
          ]

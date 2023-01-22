module WildeTest.Media.Test
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import qualified WildeTest.Media.ElementTest
import qualified WildeTest.Media.ElementSetTest


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | All tests in this package.
theTest :: Test
theTest = TestList
          [ WildeTest.Media.ElementTest.theTest
          , WildeTest.Media.ElementSetTest.theTest
          ]

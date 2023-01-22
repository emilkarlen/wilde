module WildeTest.ObjectModel.Test
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import qualified WildeTest.ObjectModel.ObjectModelTest
import qualified WildeTest.ObjectModel.UserInteraction.Test


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theTest :: Test
theTest = TestList
          [
            WildeTest.ObjectModel.ObjectModelTest.theTest
          , WildeTest.ObjectModel.UserInteraction.Test.theTest
          ]

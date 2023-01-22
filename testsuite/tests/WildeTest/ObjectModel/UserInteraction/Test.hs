module WildeTest.ObjectModel.UserInteraction.Test
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import qualified WildeTest.ObjectModel.UserInteraction.Output.Test
import qualified WildeTest.ObjectModel.UserInteraction.Input.Test
import qualified WildeTest.ObjectModel.UserInteraction.UserInteractionTest


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theTest = TestList
          [ WildeTest.ObjectModel.UserInteraction.Output.Test.theTest
          , WildeTest.ObjectModel.UserInteraction.Input.Test.theTest
          , WildeTest.ObjectModel.UserInteraction.UserInteractionTest.theTest
          ]

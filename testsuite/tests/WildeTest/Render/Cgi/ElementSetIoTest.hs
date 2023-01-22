module WildeTest.Render.Cgi.ElementSetIoTest
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.Map as Map

import Wilde.Render.Cgi.ElementSetIo

import Test.HUnit


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | All tests in this module.
theTest =
  TestLabel "Output+Input should be the identity function" $
  TestList
  [
    "Two empty sets" ~:

    checkToCgiAndBack emptyInput

  , "Two identical sets (single value)" ~:

    checkToCgiAndBack identicalSetsInput

  , "Sets with elements that have multiple values" ~:

    checkToCgiAndBack multiValueElementsInput
  ]

-------------------------------------------------------------------------------
-- A test case: Checks that the translation
--
--    Input -> CGI values -> Input
--
-- is equivalent to the identity function
-------------------------------------------------------------------------------
checkToCgiAndBack theInput = theInput ~=? (inputFromCgiValues . inputToCgiValues) theInput

-- Input in which both 'ElementSet's are empty.
emptyInput :: Input
emptyInput =
  Input
  {
    inputMedia        = Map.empty
  , customEnvironment = Map.empty
  }

-- | Input in which both 'ElementSet's are identical.
identicalSetsInput :: Input
identicalSetsInput =
  Input
  {
    inputMedia        = set
  , customEnvironment = set
  }
  where
    set = Map.fromList [("k",["v"])]

-- | Input in which there are keys who's values have multiple strings.
multiValueElementsInput :: Input
multiValueElementsInput =
  Input
  {
    inputMedia        = Map.fromList [("i",["i1","i2"]),("j",["j1"])]
  , customEnvironment = Map.fromList [("e",["e1","e2"]),("f",["f1"])]
  }

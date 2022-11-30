{-
Copyright 2013 Emil Karlén.

This file is part of Wilde.

Wilde is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Wilde is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Wilde.  If not, see <http://www.gnu.org/licenses/>.
-}

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

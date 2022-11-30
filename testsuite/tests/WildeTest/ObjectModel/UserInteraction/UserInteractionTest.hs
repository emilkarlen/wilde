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

module WildeTest.ObjectModel.UserInteraction.UserInteractionTest 
       (
         theTest
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.List
import qualified Data.Map as Map

import Test.HUnit

import           TestResources.TestUtils
import           TestResources.AssertUtils
import qualified TestResources.UserInteractionOutputMonadUtils as UiUtils
import qualified TestResources.TestData as TD
import           TestResources.UserInteractionOutputMonadUtils

import qualified Wilde.Media.GenericStringRep as Gsr

import Wilde.ObjectModel.ObjectModel
import Wilde.Media.UserInteraction.Io
import Wilde.ObjectModel.UserInteraction
import qualified Wilde.ObjectModel.UserInteraction.Common as UiCommon


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | All tests in this module.
theTest :: Test
theTest = TestList
          [ elementValueObjectNameTests
          , elementValueObjectNameListTests
          , fixedGsrTests
          ]

elementValueObjectNameTests :: Test
elementValueObjectNameTests =
  let
    objectNames = TD.elementKeyPrefixes :: [(String,ObjectName)]
  in
   identityTests
   (elementValueDecodeObjectName . elementValueEncodeObjectName)
   "elementValueObjectName"
   objectNames

elementValueObjectNameListTests :: Test
elementValueObjectNameListTests =
  let
    objectNames = TD.elementKeyPrefixes :: [(String,ObjectName)]
    nonEmptyObjectNames = filter (not . null . snd) objectNames :: [(String,ObjectName)]
    derivedObjectNameLists = concatMap permutations $
                             filter (not . null) $
                             subsequences $
                             nonEmptyObjectNames :: [[(String,ObjectName)]]
    testValues = map mkTestValue derivedObjectNameLists :: [(String,[ObjectName])]
  in
   identityTests
   (elementValueDecodeObjectNameList . elementValueEncodeObjectNameList)
   "elementValueObjectNameList"
   testValues
  where
    mkTestValue :: [(String,ObjectName)] -> (String,[ObjectName])
    mkTestValue l =
      let
        (testNames,objectNames) = unzip l
      in
       (intercalate "," testNames,objectNames)

-- Would be better to test output combined with input in the context of
-- a web server.  The reason is that the server
-- do a slight transformation on the variables in the form.
fixedGsrTests ::Test
fixedGsrTests =
  TestLabel "Tests outputing and inputing fixed value (in form of Generic String Rep) to/from the environment" $
  TestList
  [
    "no fixed-gsr when flag is not present" ~:
    Nothing `isExpectedAsFixedGsrInEnvironment` []

  , "single fixed-gsr value" ~:
    (Just aNonEmptyValue)  
    `isExpectedAsFixedGsrInEnvironment` 
    (UiCommon.metaValuesForRole UiCommon.Fix fixedGsr_atName fixedGsr_oName aNonEmptyValue)
  ]
  where
    aNonEmptyValue = "expectedValue"

fixedGsr_value = UiCommon.elementKeyForAttributeValue
                 fixedGsr_atName 
                 fixedGsr_oName

fixedGsr_indicator = UiCommon.elementKeyForRoleIndicator
                     fixedGsr_atName 
                     fixedGsr_oName

fixedGsr_oName :: ObjectName
fixedGsr_oName = ["o"]

fixedGsr_atName :: AttributeName
fixedGsr_atName = "a"

isExpectedAsFixedGsrInEnvironment :: Maybe Gsr.GenericStringRep
                                  -> [Element]
                                  -> Test
isExpectedAsFixedGsrInEnvironment expectedResult elementsInSet =
  TestCase $
  check env assertion m
  where
    env       = emptyEnv `UiUtils.withMedia` media
    media    :: Map.Map String [String]
    media     = Map.fromList $ map (\(ek,v) -> (elementKeyRender ek,[v])) elementsInSet
    assertion = failOnError
                (\actualResult -> assertEqual
                                  "" 
                                  expectedResult 
                                  actualResult)
    m         = UiCommon.inputFixedFromEnv fixedGsr_atName fixedGsr_oName

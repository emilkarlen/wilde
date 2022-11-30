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

module WildeTest.ObjectModel.ObjectModelTest
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import Wilde.ObjectModel.ObjectModel

import qualified Wilde.ObjectModel.AttributeTypeListSetup.SansAnnotation as AttributeTypeListSetup

import Wilde.ApplicationConstruction.ObjectModel.ObjectType

import qualified TestResources.TestData as TD

import Wilde.ObjectModel.ObjectModelUtils

import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.DdlAtAnnotation as DdlAtAnnotation


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | All tests in this module.
theTest =
  TestList
  [ oGetAttributesOfObjectFunTest
  ]

oGetAttributesOfObjectFunTest :: Test
oGetAttributesOfObjectFunTest =
  TestLabel "otAttributeTypeListSetup" $
  TestList [
    TestLabel "Valid" (
       TestList [
          testValid "empty list" [],
          testValid "singleton list" [validAt1],
          testValid "2 elem list"    [validAt1,validAt1],
          testValid "same order as in OT"     [validAt1,validAt2],
          testValid "reversed order as in OT" [validAt2,validAt1],
          testValid "many duplicates"         [validAt2,validAt1,validAt1,validAt2,validAt1]
          ]),
    TestLabel "Invalid" (
      TestList [
         testInvalid "singleton"             [invalidAt],
         testInvalid "one valid one invalid" [validAt1,invalidAt]
         ])
    ]
  where
    testValid,testInvalid :: String -> [Any (AttributeType DdlAtAnnotation.Configuration TD.PkNameTable)] -> Test
    testValid   = test assertValid
    testInvalid = test $ \ats -> assertBool "not left"  (isLeft  (funToTest ats))

    test :: ([Any (AttributeType DdlAtAnnotation.Configuration TD.PkNameTable)] -> Assertion)
          -> String
          -> [Any (AttributeType DdlAtAnnotation.Configuration TD.PkNameTable)] -> Test
    test assertion name ats = TestLabel name $ TestCase (assertion ats)

    validAt1   = Any TD.at_pk
    validAt2   = Any TD.at_name
    invalidAt  = Any $ at_PrimaryKeyType TD.inputWidth TD.ColumnNotUsedByAnyAtInOt noDefault "Invalid"

funToTest = AttributeTypeListSetup.mk TD.otPkName

isLeft  = either (const True) (const False)

assertValid :: [Any (AttributeType DdlAtAnnotation.Configuration TD.PkNameTable)] -> Assertion
assertValid ats =
    case funToTest ats of
      -- Check result is Right.
      Left (Any at) ->
        assertFailure ("Expected Right, got Left: " ++ atCrossRefKey at)
      Right atListSetup ->
        -- Check number of attributes and their order.
        let
          attrs                = AttributeTypeListSetup.apply atListSetup TD.oPkName1
          crossRefKeysExpected = mapAttributeTypeAnyValue atCrossRefKey ats
                                 :: [String]
          crossRefKeysActual   = mapAttributeAnyValue (atCrossRefKey . attrType) attrs
                                 :: [String]
        in
         assertEqual
         ("Expected " ++ show crossRefKeysExpected)
         crossRefKeysExpected crossRefKeysActual

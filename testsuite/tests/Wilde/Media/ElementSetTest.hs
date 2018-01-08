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

module Wilde.Media.ElementSetTest
       (
         theTest
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import qualified Wilde.Media.ElementSet as ES


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | All tests in this module.
theTest :: Test
theTest = TestLabel "WildeSqlInputer.inputExpression" $
          TestList
          [
            trimEmptyIsNothingTests
          , trimEmptyIsMissingTests
          ]


trimEmptyIsNothingTests :: Test
trimEmptyIsNothingTests = TestLabel "trimEmptyIsNothing" $
          TestList
          [
            "empty string" ~:

            checkOk
            ES.trimEmptyIsNothing
            Nothing
            ""

          , "only whitespace" ~:

            checkOk
            ES.trimEmptyIsNothing
            Nothing
            "   \n\t  "
          ]

trimEmptyIsMissingTests :: Test
trimEmptyIsMissingTests = TestLabel "trimEmptyIsMissing" $
          TestList
          [
            "empty string" ~:

            checkMissing
            ES.trimEmptyIsMissing
            ES.ValueMissing
            ""

          , "only whitespace" ~:

            checkMissing
            ES.trimEmptyIsMissing
            ES.ValueMissing
            "   \n\t  "
          ]


-------------------------------------------------------------------------------
-- Parses the given String and checks that the parse result is OK
-- and also that it is equal to the given SQL Expression.
-------------------------------------------------------------------------------
checkOk :: (Eq b, Show b)
        => ES.Parser a b
        -> b
        -> a
        -> Assertion
checkOk parser expected input =
  case parser input of
    Left  errMsg -> assertFailure ("Failure: " ++ (show errMsg))
    Right actual -> assertEqual "OK" expected actual

checkMissing :: (Eq b, Show b)
             => ES.Parser a b
             -> ES.ElementLookupErrorType
             -> a
             -> Assertion
checkMissing parser expected input =
  case parser input of
    Right ok -> assertFailure ("Failure: OK (expected error)")
    Left actual -> assertEqual "Error:" expected actual

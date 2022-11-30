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

module WildeTest.ApplicationConstruction.StandardServices.WildeSqlInputerTest
       (
         theTest
       )
       where

-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


-- import qualified Data.Map as Map

-- import Data.Time.Calendar

import Test.HUnit

import qualified Wilde.Media.ElementSet as ES

import qualified Wilde.Database.Sql as WSql

import qualified TestResources.TestData as TD
import qualified Wilde.ApplicationConstruction.UserInteraction.Input.WildeSqlInputer as WildeSqlInputer


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | All tests in this module.
theTest :: Test
theTest = TestLabel "WildeSqlInputer.inputExpression" $
          TestList
          [
            okSimpleExpressionTests

          , okComplexExpressionTests

          ]

okSimpleExpressionTests :: Test
okSimpleExpressionTests = TestLabel "Parsing of simple values = non-complex values" $
          TestList
          [
            "parse column" ~:

            check
            colIdentPk
            (WSql.SqlExprField Nothing TD.ColumnPk)

          -- , "parse column (non-existing)" ~:

          --   checkError
          --   (parse "non_existing_column")

          , "NULL" ~:

            check
            "null"
            (WSql.const_null)

          , "Boolean (true)" ~:

            check
            "true"
            (WSql.const_bool True)

          , "Integer" ~:

            check
            "55"
            (WSql.const_int 55)

          , "Float" ~:

            check
            "55.6"
            (WSql.const_float 55.6)

          , "Char" ~:

            check
            "'c'"
            (WSql.const_char 'c')

          , "Integer" ~:

            check
            "55"
            (WSql.const_int 55)

          , "Float" ~:

            check
            "55.05"
            (WSql.const_float 55.05)

          , "String" ~:

            check
            "\"A string\""
            (WSql.const_string "A string")

          -- , "parenthesis" ~:

          --   check
          --   (ExprsInParens [ColumnVar TD.ColumnPk])
          --   (parse $ "( " ++ colIdentPk ++ "  )")

          -- , "IN (no elements)" ~:

          --   check
          --   (In Null (ExprsInParens []))
          --   (parse $ "NULL  IN  () ")

          -- , "IN (some elements)" ~:

          --   check
          --   (In Null (ExprsInParens [BoolLiteral False
          --                           ,Plus (IntLiteral 1) (IntLiteral 2)
          --                           ,ColumnVar TD.ColumnPk]))
          --   (parse $ "NULL  IN  (false,1 + 2," ++ colIdentPk ++ ") ")


          ]

okComplexExpressionTests :: Test
okComplexExpressionTests = TestLabel "Parsing of complex values = non-simple values" $
          TestList
          [
          -- , "parse column (non-existing)" ~:

          --   checkError
          --   (parse "non_existing_column")

            "function call (without arguments)" ~:

            check
            "myFunc ()"
            (WSql.SqlExprFun "myFunc" [])

          , "function call (with arguments)" ~:

            check
            "myFunc (TRUE,NULL)"
            (WSql.SqlExprFun "myFunc" [WSql.const_bool True,
                                       WSql.const_null])

          -- , "parenthesis" ~:

          --   check
          --   (ExprsInParens [ColumnVar TD.ColumnPk])
          --   (parse $ "( " ++ colIdentPk ++ "  )")

          -- , "IN (no elements)" ~:

          --   check
          --   (In Null (ExprsInParens []))
          --   (parse $ "NULL  IN  () ")

          -- , "IN (some elements)" ~:

          --   check
          --   (In Null (ExprsInParens [BoolLiteral False
          --                           ,Plus (IntLiteral 1) (IntLiteral 2)
          --                           ,ColumnVar TD.ColumnPk]))
          --   (parse $ "NULL  IN  (false,1 + 2," ++ colIdentPk ++ ") ")


          ]



-------------------------------------------------------------------------------
-- Parses the given String and checks that the parse result is OK
-- and also that it is equal to the given SQL Expression.
-------------------------------------------------------------------------------
check :: String   -- ^ String to parse
      -> OkResult -- ^ Expected result
      -> Assertion
check input expected =
  case parse input of
    Left  errMsg -> assertFailure ("Failure: " ++ (show errMsg))
    Right actual -> assertEqual "OK" expected actual

parse :: String
      -> ParseResult
parse s = WildeSqlInputer.expression_mandatory TD.otPkName s

type ParseResult = Either ES.ElementLookupErrorType OkResult

type OkResult = WSql.SqlExpr TD.PkNameTable

colIdentPk,colIdentName :: String
colIdentPk   = "columnpk"
colIdentName = "columnname"

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

module WildeTest.ApplicationConstruction.SqlExprParserTest
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import Text.Parsec.Error (ParseError)

import qualified TestResources.TestData as TD
import Wilde.ApplicationConstruction.UserInteraction.Input.SqlExprParser


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | All tests in this module.
theTest :: Test
theTest = TestLabel "SqlExprParserTest" $
          TestList
          [
            "parse string (simple)" ~:
            
            checkOk 
            (StringLiteral "A string")
            (parse "\"A string\"")
            
          , "parse column" ~:
            
            checkOk 
            (ColumnVar TD.ColumnPk)
            (parse colIdentPk)
            
          , "parse column (non-existing)" ~:
            
            checkError
            (parse "non_existing_column")
            
          , "function call (without arguments)" ~:
            
            checkOk 
            (FunCall "myFunc" [])
            (parse $ "myFunc ()")
            
          , "function call (with arguments)" ~:
            
            checkOk 
            (FunCall "myFunc" [BoolLiteral True,Null])
            (parse $ "myFunc (TRUE,NULL)")
          
          , "parenthesis" ~:
            
            checkOk 
            (ExprsInParens [ColumnVar TD.ColumnPk])
            (parse $ "( " ++ colIdentPk ++ "  )")
            
          , "IN (no elements)" ~:
            
            checkOk 
            (In Null (ExprsInParens []))
            (parse $ "NULL  IN  () ")
            
          , "IN (some elements)" ~:
            
            checkOk 
            (In Null (ExprsInParens [BoolLiteral False
                                    ,Plus (IntLiteral 1) (IntLiteral 2)
                                    ,ColumnVar TD.ColumnPk]))
            (parse $ "NULL  IN  (false,1 + 2," ++ colIdentPk ++ ") ")
            
            
          ]


parse :: String 
         -> Either ParseError (Expression TD.PkNameTable)
parse = parseWithListedColumns columns

colIdentPk,colIdentName :: String
colIdentPk   = "columnpk"
colIdentName = "columnname"

columns :: [(String,TD.PkNameTable)]
columns = [(colIdentPk,TD.ColumnPk)
          ,(colIdentName,TD.ColumnName)
          ]


checkOk :: (Eq tableColumn,Show tableColumn)
        => Expression tableColumn 
        -> Either ParseError (Expression tableColumn)
        -> Assertion
checkOk _ (Left errMsg)         = assertFailure ("Failure: " ++ (show errMsg))
checkOk expected (Right actual) = assertEqual "OK" expected actual

checkError :: (Eq tableColumn,Show tableColumn)
           => Either ParseError (Expression tableColumn)
           -> Assertion
checkError (Left errMsg)  = return ()
checkError (Right actual) = assertFailure $
                            "Unexpected OK result: " ++ (show actual)


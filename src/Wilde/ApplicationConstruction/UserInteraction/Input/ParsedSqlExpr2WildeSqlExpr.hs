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

-------------------------------------------------------------------------------
-- | A function that translates an SQL expression, as parsed by
-- "SqlExprParser"
-- to the format used by Wilde for sending SQL to the database.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.UserInteraction.Input.ParsedSqlExpr2WildeSqlExpr
       (
         translate,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Database.Sql as WSql

import Wilde.ApplicationConstruction.UserInteraction.Input.SqlExprParser as PSql


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


translate :: PSql.Expression tableColumn -> WSql.SqlExpr tableColumn
translate (Null)                  = WSql.const_null
translate (IntLiteral    x)       = WSql.const_int x
translate (FloatLiteral  x)       = WSql.const_float x
translate (BoolLiteral   x)       = WSql.const_bool x
translate (StringLiteral x)       = WSql.const_string x
translate (CharLiteral   x)       = WSql.const_char x
translate (Pow   exprL exprR)     = bin "^"    exprL exprR
translate (Mul   exprL exprR)     = bin "*"    exprL exprR
translate (Div   exprL exprR)     = bin "/"    exprL exprR
translate (Mod   exprL exprR)     = bin "%"    exprL exprR
translate (Plus  exprL exprR)     = bin "+"    exprL exprR
translate (Minus exprL exprR)     = bin "-"    exprL exprR
translate (Lt    exprL exprR)     = bin "<"    exprL exprR
translate (Gt    exprL exprR)     = bin ">"    exprL exprR
translate (Lte   exprL exprR)     = bin "<="   exprL exprR
translate (Gte   exprL exprR)     = bin ">="   exprL exprR
translate (Eq    exprL exprR)     = bin "="    exprL exprR
translate (Is    exprL exprR)     = bin "IS"   exprL exprR
translate (Like  exprL exprR)     = bin "LIKE" exprL exprR
translate (And   exprL exprR)     = bin "AND"  exprL exprR
translate (Or    exprL exprR)     = bin "OR"   exprL exprR
translate (In    exprL exprR)     = bin "IN"   exprL exprR
translate (Pos   expr)            = unPrefix "+"   expr
translate (Neg   expr)            = unPrefix "-"   expr
translate (Not   expr)            = unPrefix "NOT" expr
translate (ExprsInParens exprs)   = WSql.paren $ map translate exprs
translate (ColumnVar tableColumn) = WSql.field tableColumn
translate (FunCall funName exprs) = WSql.fun funName $ map translate exprs
  
-- | Helper for binary operators.
bin :: String -- ^ operator
       -> PSql.Expression tableColumn -- ^ Left operand
       -> PSql.Expression tableColumn -- ^ Right operand
       -> WSql.SqlExpr tableColumn
bin oper exprL exprR = WSql.binOp
                       (WSql.SqlBinOpAny oper)
                       (translate exprL)
                       (translate exprR)

-- | Helper for unary prefix operators.
unPrefix :: String -- ^ operator
            -> PSql.Expression tableColumn -- ^ operand
            -> WSql.SqlExpr tableColumn
unPrefix oper expr = WSql.unOp
                     (WSql.SqlUnOpPrefixAny oper) 
                     (translate expr)

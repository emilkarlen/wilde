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

{-# LANGUAGE FlexibleInstances #-}

-------------------------------------------------------------------------------
-- | Simple Abstract Syntax for SQL.
-------------------------------------------------------------------------------
module Wilde.Database.Sql
       (
         -- * Types and type classes

         SqlIdentifier,
         SQL_IDENTIFIER(..),
         SQL_STRING(..),

         -- DOC(..), -- behövs?

         SqlDmlStatement(..),
         
         -- * Expression

         SqlExpr(..),
         SqlBinOp(..),
         SqlUnOp(..),

         SqlConstant(..),
         
         -- ** Constants

         const_null,
         const_bool,
         const_int,
         const_float,
         const_string,
         const_char,

         -- ** non-Constants

         field,
         tableField,
         posParam,
         binOp,
         unOp,
         fun,
         paren,
         any_expr,

         eqOp,
         ltOp,
         gtOp,
         andOp,
         inOp,

         eq,
         lt,
         gt,
         sql_and,
         inoper,

         isNull,
         isNotNull,

         -- * Select

         SqlSelect(..),
         select,

         -- ** Joins

         SqlJoin(..),
         SqlJoinTableAndType(..),
         
         OuterJoinType,

         outerNothing,
         outerLeft,
         outerRight,
         outerBoth,

         -- * Insert

         SqlInsert(..),
         insert,

         -- * Update

         SqlUpdate(..),
         update,

         -- * Delete

         SqlDelete(..),
         delete,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Utils.NonEmptyList as NonEmpty


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Types -
-------------------------------------------------------------------------------


-- | Tables, and columns.
type SqlIdentifier = String

class SQL_IDENTIFIER a where
  sqlIdentifier :: a -> SqlIdentifier

instance SQL_IDENTIFIER [Char] where
  sqlIdentifier = id

-- | Class for values that are used in SQL structures, which must be
-- convertible to strings. (Since SQL are executed as strings.)
class SQL_STRING a where
  sqlString :: a -> String

-- | Any DML statement.
data SqlDmlStatement fieldType
  = SqlDmlSelect (SqlSelect fieldType)
  | SqlDmlInsert (SqlInsert fieldType)
  | SqlDmlUpdate (SqlUpdate fieldType)
  | SqlDmlDelete (SqlDelete fieldType)

instance Functor SqlDmlStatement where
  fmap f (SqlDmlSelect x) = SqlDmlSelect $ fmap f x
  fmap f (SqlDmlInsert x) = SqlDmlInsert $ fmap f x
  fmap f (SqlDmlUpdate x) = SqlDmlUpdate $ fmap f x
  fmap f (SqlDmlDelete x) = SqlDmlDelete $ fmap f x

-- | Simple SELECT statement, complex enough for the current needs.
-- The type is parametrized by the type used to represent a table column.
-- This makes the type a bit more complex, but lets us use values of a specific
-- type instead of strings, which makes type checking a bit more powerfull.
data SqlSelect fieldType =
  SqlSelect
  {
    sqlSelTableName     :: SqlIdentifier,
    sqlSelJoins         :: [SqlJoin fieldType],
    sqlSelSelectionList :: [SqlExpr fieldType],
    sqlSelWhere         :: (Maybe (SqlExpr fieldType)),
    sqlSelOrderBy       :: [SqlExpr fieldType]
  }
  deriving (Eq,Show)

select = SqlSelect

type OuterJoinType = Maybe (Maybe (Either () ()))

-- | Constants for the types of outer join.

-- | Inner join (no outer join).
outerNothing :: OuterJoinType

-- | Lefter outer, right inner join.
outerLeft    :: OuterJoinType

-- | Right outer, left inner join.
outerRight   :: OuterJoinType

-- | Left and right outer.
outerBoth    :: OuterJoinType

outerNothing = Nothing
outerLeft    = Just $ Just (Left ())
outerRight   = Just $ Just (Right ())
outerBoth    = Just Nothing

-- | A \"right\" table to join.
--
-- \"right\" is the position for OUTER/INNER joins.
data SqlJoin fieldType = 
  SqlJoin
  {
    sqlJoinTableAndType :: SqlJoinTableAndType
  , sqlJoinExpr         :: SqlExpr fieldType
  }
  deriving (Eq,Show)

-- | A part of the information about a join that is
-- used in more than one place.
data SqlJoinTableAndType =
  SqlJoinTableAndType
  {
    sqljttTable :: SqlIdentifier
  , sqljttAlias :: Maybe SqlIdentifier
  , sqljttType  :: OuterJoinType
  }
  deriving (Eq,Show)

instance Functor SqlJoin where
  fmap f sj@(SqlJoin { sqlJoinExpr = theExpr }) =
    sj { sqlJoinExpr = fmap f theExpr }

instance Functor SqlSelect where
  fmap f (SqlSelect str joins exprs mbWhereExpr orderByExprs) =
    SqlSelect str (map  (fmap f) joins)
                  (fmap (fmap f) exprs)
                  (fmap (fmap f) mbWhereExpr)
                  (fmap (fmap f) orderByExprs)

--
-- The types here should probably be replaced by equivalent data types
-- from a SQL library.
--
-- The types are included here just for quick experimentation.
--
-------------------------------------------------------------------------------  
-- | Parametrize by the field type.  This will make SQL expressions
-- related to a special field type.
-- A drawback is that field types must be defined for each SQL SELECT
-- field list.  The future will show if this will be removed or not.
--
-- The expression is untyped in the type of the value of the expression.
-- (Perhaps this can be acomplished using Generalized Abstract Datatypes).
-------------------------------------------------------------------------------
data SqlExpr fieldType = SqlExprConst        SqlConstant
                       | SqlExprField        (Maybe SqlIdentifier) fieldType -- [tbl.]col
                       | SqlExprPosParam
                       | SqlExprBinOp        SqlBinOp          (SqlExpr fieldType) (SqlExpr fieldType)
                       | SqlExprUnOp         SqlUnOp           (SqlExpr fieldType)
                       | SqlExprFun          String            [SqlExpr fieldType]
                         -- | Either a set or a parenthasized expression.
                       | SqlExprParen        [SqlExpr fieldType]
                       | SqlExprAny          String -- ^ Awful way to include arbitrary string as an Expression
                       deriving (Eq,Show)

instance Functor SqlExpr where
  fmap f (SqlExprConst c)                  = SqlExprConst        c
  fmap f (SqlExprField mbTbl field)        = SqlExprField        mbTbl (f field)
  fmap f (SqlExprPosParam)                 = SqlExprPosParam
  fmap f (SqlExprBinOp        bop opL opR) = SqlExprBinOp        bop (fmap f opL) (fmap f opR)
  fmap f (SqlExprUnOp  uop op)             = SqlExprUnOp         uop (fmap f op)
  fmap f (SqlExprFun          fun args)    = SqlExprFun          fun (fmap (fmap f) args)
  fmap f (SqlExprParen        exprs)       = SqlExprParen        (map (fmap f) exprs)
  fmap f (SqlExprAny          s)           = SqlExprAny          s

-- | All types of constants.
data SqlConstant
  = SqlConstNull
  | SqlConstBool   Bool
  | SqlConstChar   Char
  | SqlConstInt    Integer
  | SqlConstFloat  Double
  | SqlConstString String
    deriving (Eq,Show,Read)

-- | The NULL constant
const_null :: SqlExpr a
const_null = SqlExprConst SqlConstNull

-- | An expression that is a boolean constant.
const_bool :: Bool -> SqlExpr a
const_bool b = SqlExprConst $ SqlConstBool b

-- | An expression that is an integer constant.
const_int :: Integer -> SqlExpr a
const_int x = SqlExprConst $ SqlConstInt x

-- | An expression that is a floating point constant.
const_float :: Double -> SqlExpr a
const_float x = SqlExprConst $ SqlConstFloat x

-- | An expression that is a string constant.
const_string :: String -- ^ Unquoted string
                -> SqlExpr a
const_string s = SqlExprConst $ SqlConstString s

-- | An expression that is a character constant.
const_char :: Char
              -> SqlExpr a
const_char ch = SqlExprConst $ SqlConstChar ch

field              = SqlExprField Nothing
tableField tbl col = SqlExprField (Just tbl) col
posParam           = SqlExprPosParam
binOp              = SqlExprBinOp
unOp               = SqlExprUnOp
fun                = SqlExprFun
paren              = SqlExprParen
any_expr           = SqlExprAny

data SqlBinOp = SqlBinOpEq
              | SqlBinOpLt
              | SqlBinOpGt
              | SqlBinOpAnd
              | SqlBinOpIn
              | SqlBinOpAny String -- ^ Holder for arbitrary operator.
              deriving (Eq,Show)

-- | Testing with shorter names.
eqOp   = SqlBinOpEq
ltOp   = SqlBinOpLt
gtOp   = SqlBinOpGt
andOp  = SqlBinOpAnd
inOp   = SqlBinOpIn

eq,lt,gt,sql_and,inoper :: SqlExpr fieldType -> SqlExpr fieldType -> SqlExpr fieldType
eq      = SqlExprBinOp SqlBinOpEq
lt      = SqlExprBinOp SqlBinOpLt
gt      = SqlExprBinOp SqlBinOpGt
sql_and = SqlExprBinOp SqlBinOpAnd
inoper  = SqlExprBinOp SqlBinOpIn


data SqlUnOp = SqlUnOpPrefixAny String
             | SqlUnOpPostfixIsNull
             | SqlUnOpPostfixIsNotNull
             deriving (Eq,Show)

isNull,isNotNull :: SqlExpr fieldType -> SqlExpr fieldType
isNull    = SqlExprUnOp SqlUnOpPostfixIsNull
isNotNull = SqlExprUnOp SqlUnOpPostfixIsNotNull

instance SQL_STRING SqlBinOp where
  sqlString SqlBinOpEq      = "="
  sqlString SqlBinOpLt      = "<"
  sqlString SqlBinOpGt      = ">"
  sqlString SqlBinOpAnd     = "AND"
  sqlString SqlBinOpIn      = "IN"
  sqlString (SqlBinOpAny s) = s

instance SQL_STRING SqlUnOp where
  sqlString (SqlUnOpPrefixAny s)    = s
  sqlString SqlUnOpPostfixIsNull    = "IS NULL"
  sqlString SqlUnOpPostfixIsNotNull = "IS NOT NULL"

-- class SQL_FIELD_PARAM_EXPR a where
--   mkFieldParamExpr :: String -> a -> String

-- instance SQL_FIELD_PARAM_EXPR SqlBinOp where
--   mkFieldParamExpr field oper = field ++ " " ++ (show oper) ++ " ?"

-- instance SQL_FIELD_PARAM_EXPR SqlPostFixOp where
--   mkFieldParamExpr field oper = field ++ " " ++ (show oper)

-- instance SQL_FIELD_PARAM_EXPR SqlOp where
--   mkFieldParamExpr field (SqlOpBin     oper) = mkFieldParamExpr field oper
--   mkFieldParamExpr field (SqlOpPostFix oper) = mkFieldParamExpr field oper


-------------------------------------------------------------------------------
-- - insert -
-------------------------------------------------------------------------------


data SqlInsert fieldType =
  SqlInsert
  {
    insertTable   :: SqlIdentifier,
    insertColumns :: [fieldType],
    insertExprs   :: [SqlExpr fieldType]
  }

insert = SqlInsert

instance Functor SqlInsert where
  fmap f ins@(SqlInsert tbl cols exprs) =
    ins {
      insertColumns = fmap f cols,
      insertExprs   = fmap (fmap f) exprs
      }


-------------------------------------------------------------------------------
-- - update -
-------------------------------------------------------------------------------


data SqlUpdate fieldType =
  SqlUpdate
  {
    updateTable   :: SqlIdentifier,
    updateColumns :: NonEmpty.List (fieldType,SqlExpr fieldType),
    updateWhere   :: Maybe (SqlExpr fieldType)
   }

update = SqlUpdate

instance Functor SqlUpdate where
  fmap f x@(SqlUpdate tbl cols mbExpr) =
    x {
      updateColumns = fmap (\(col,expr) -> (f col,fmap f expr)) cols,
      updateWhere   = fmap (fmap f) mbExpr
      }


-------------------------------------------------------------------------------
-- - delete -
-------------------------------------------------------------------------------


data SqlDelete fieldType =
  SqlDelete
  {
    deleteTable   :: SqlIdentifier,
    deleteWhere   :: Maybe (SqlExpr fieldType)
   }

delete = SqlDelete

instance Functor SqlDelete where
  fmap f x@(SqlDelete tbl mbExpr) =
    x {
      deleteWhere   = fmap (fmap f) mbExpr
      }

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

-- | Misc utilities for constructing SQL statements.
module Wilde.Database.SqlMisc
       (
         module Wilde.Database.Sql,

         selectRep,

         ands,
         andsNonEmpty,
         eqPosParamAnds,
         eqPosParamAndsNonEmpty,
         eqFieldPosParamAndsNonEmpty,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


--import Wilde.Utils.Utils

import qualified Wilde.Utils.NonEmptyList as NonEmpty

import Wilde.Database.Sql


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | SQL for selection from the "representation" ("Rep") version of the
-- table.  This means that all columns are selected from the table without
-- any joins.
selectRep :: SqlIdentifier         -- ^ Table name
          -> [field]               -- ^ The fields to SELECT
          -> Maybe (SqlExpr field) -- ^ WHERE expression
          -> [field]               -- ^ ORDER BY
          -> SqlSelect field
selectRep tableName fields mbSqlExpr orderByFields =
  let selExprs       = map field fields
      mbWhereExpr    = mbSqlExpr
      orderByExprs   = map field orderByFields
  in  SqlSelect tableName [] selExprs mbWhereExpr orderByExprs

-- Implementation note: Does it matter if the fold is left or right?
ands :: [SqlExpr fieldType]
        -> SqlExpr fieldType
ands l = foldl1 (SqlExprBinOp SqlBinOpAnd) l

andsNonEmpty :: NonEmpty.List (SqlExpr fieldType)
                -> SqlExpr fieldType
andsNonEmpty l = NonEmpty.foldl1 (SqlExprBinOp SqlBinOpAnd) l

eqFieldPosParamAndsNonEmpty :: NonEmpty.List (fieldType) -> SqlExpr fieldType
eqFieldPosParamAndsNonEmpty = andsNonEmpty . fmap (\f -> (field f) `eq` posParam)

eqPosParamAndsNonEmpty :: NonEmpty.List (SqlExpr fieldType) -> SqlExpr fieldType
eqPosParamAndsNonEmpty = andsNonEmpty . fmap (\e -> e `eq` posParam)

eqPosParamAnds :: [fieldType] -> SqlExpr fieldType
eqPosParamAnds = ands . map (\f -> (field f) `eq` posParam)

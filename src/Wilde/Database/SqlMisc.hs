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

import qualified Data.List.NonEmpty as NonEmpty

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

andsNonEmpty :: NonEmpty.NonEmpty (SqlExpr fieldType)
                -> SqlExpr fieldType
andsNonEmpty l = foldl1 (SqlExprBinOp SqlBinOpAnd) l

eqFieldPosParamAndsNonEmpty :: NonEmpty.NonEmpty (fieldType) -> SqlExpr fieldType
eqFieldPosParamAndsNonEmpty = andsNonEmpty . fmap (\f -> (field f) `eq` posParam)

eqPosParamAndsNonEmpty :: NonEmpty.NonEmpty (SqlExpr fieldType) -> SqlExpr fieldType
eqPosParamAndsNonEmpty = andsNonEmpty . fmap (\e -> e `eq` posParam)

eqPosParamAnds :: [fieldType] -> SqlExpr fieldType
eqPosParamAnds = ands . map (\f -> (field f) `eq` posParam)

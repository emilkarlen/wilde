module Wilde.Database.DmlRendering
       (
         DOC(..),
         renderDoc,
         RENDER_SQL(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.List.NonEmpty as NonEmpty

import Text.PrettyPrint

import Wilde.Database.Sql

import Prelude hiding ((<>))


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - classes -
-------------------------------------------------------------------------------


-- | Simple class so that we can reuse the name of the Doc constructor.
class DOC a where
  docOf :: a -> Doc

renderDoc :: DOC a => a -> String
renderDoc  = render . docOf

class RENDER_SQL a where
  renderSql :: a -> String


-------------------------------------------------------------------------------
-- - Rendering of expressions -
-------------------------------------------------------------------------------


instance DOC SqlConstant where
  docOf SqlConstNull       = text "NULL"
  docOf (SqlConstBool x)   = text $ show x
  docOf (SqlConstChar x)   = text $ '\'' : x : "'" -- TODO: Escape special characters.
  docOf (SqlConstInt x)    = text $ show x
  docOf (SqlConstFloat x)  = text $ show x
  docOf (SqlConstString x) = text $ '\'' : x ++ "'" -- TODO: Escape special characters.

instance DOC SqlBinOp where
  docOf = text . sqlString

instance DOC SqlUnOp where
  docOf = text . sqlString

instance SQL_IDENTIFIER fieldType => DOC (SqlExpr fieldType) where
  docOf (SqlExprConst c)                = docOf c
  docOf (SqlExprField Nothing    field) = text $ sqlIdentifier field
  docOf (SqlExprField (Just tbl) field) = hcat $ punctuate (char '.') [text s | s <- [tbl,sqlIdentifier field]]
  docOf (SqlExprPosParam)               = char '?'
  -- Special case for AND just to make SQL a bit more readable: each
  -- expression is on a separate line).
  -- (AND operators are often used in such a way that this makes the
  -- SQL more readable.)
  docOf (SqlExprBinOp SqlBinOpAnd    sqlExprL sqlExprR) = docOf sqlExprL <+> docOf SqlBinOpAnd $+$ docOf sqlExprR
  docOf (SqlExprBinOp sqlBinOp       sqlExprL sqlExprR) = hsep [docOf sqlExprL,docOf sqlBinOp,docOf sqlExprR]
  docOf (SqlExprUnOp  sqlUnOp        sqlExpr)           = hsep [docOf sqlExpr,docOf sqlUnOp]
  docOf (SqlExprFun   fnName         sqlExprs)          = let argList = hcat $ punctuate (char ',') $ map docOf sqlExprs
                                                          in  text fnName <> parens argList
  docOf (SqlExprParen exprs)                            = let elements = hcat $ punctuate (char ',') $ map docOf exprs
                                                          in  parens elements
  docOf (SqlExprAny s)                                  = text s


-------------------------------------------------------------------------------
-- - DML -
-------------------------------------------------------------------------------


instance SQL_IDENTIFIER columnType => RENDER_SQL (SqlDmlStatement columnType) where
  renderSql = renderDoc

instance SQL_IDENTIFIER columnType => DOC (SqlDmlStatement columnType) where
  docOf (SqlDmlSelect stmt) = docOf stmt
  docOf (SqlDmlInsert stmt) = docOf stmt
  docOf (SqlDmlUpdate stmt) = docOf stmt
  docOf (SqlDmlDelete stmt) = docOf stmt


-------------------------------------------------------------------------------
-- - SELECT -
-------------------------------------------------------------------------------


instance SQL_IDENTIFIER columnType => RENDER_SQL (SqlSelect columnType) where
  renderSql = renderDoc

instance SQL_IDENTIFIER fieldType => DOC (SqlSelect fieldType) where
  docOf (SqlSelect tableName joins selExprs mbWhereExpr orderByExprss) =
    selDoc   $+$
    fromDoc  $+$
    whereDoc $+$
    ordByDoc
    where
    formatJoin :: SQL_IDENTIFIER fieldType => SqlJoin fieldType -> Doc
    formatJoin (SqlJoin (SqlJoinTableAndType table mbAlias outerInfo) expr) =
      let
        tableDoc = text table
        aliasDoc = maybe empty (\s -> text "AS" <+> text s) mbAlias
        exprDoc  = text "ON" <+> docOf expr
      in
       (outerTypeDoc outerInfo) <+> text "JOIN" <+> (hcat $ punctuate (char ' ') [tableDoc,aliasDoc,exprDoc])

    outerTypeDoc :: OuterJoinType -> Doc
    outerTypeDoc Nothing      = empty
    outerTypeDoc (Just outer) = leftOrRight <+> text "OUTER"
      where
        leftOrRight = case outer of
          Nothing -> empty
          Just lr -> text (either (const "LEFT") (const "RIGHT") lr)
    selDoc      = text "SELECT" $+$ myNest 1 (commaAndLineSep selExprs)
    joinDocs    = map formatJoin joins
    fromTblRefs = (text tableName) : joinDocs
    fromDoc     = text "FROM" $+$ myNest 1 (vcat fromTblRefs)
    whereDoc    = case mbWhereExpr of
      Nothing    -> empty
      Just expr  -> text "WHERE" $+$ myNest 1 (docOf expr)
    ordByDoc    = case orderByExprss of
      []    -> empty
      exprs -> text "ORDER BY" $+$ myNest 1 (commaAndLineSep exprs)

-- | Lists elements one per line, separated by comma.
commaAndLineSep :: DOC a => [a] -> Doc
commaAndLineSep xs = vcat $ punctuate comma $ map docOf xs

-- | Lists elements one per line, separated by comma.
commaAndLineSepDocs :: [Doc] -> Doc
commaAndLineSepDocs xs = vcat $ punctuate comma xs

-- | Variant of nest that uses my preference of
-- indent spaces.
myNest :: Int -> Doc -> Doc
myNest n = nest (4*n)


-------------------------------------------------------------------------------
-- - INSERT -
-------------------------------------------------------------------------------


instance SQL_IDENTIFIER fieldType => DOC (SqlInsert fieldType) where
  docOf (SqlInsert tbl cols exprs) =
    let
      colDocs = hcat $ punctuate comma $ map (text . sqlIdentifier) cols
    in
     text "INSERT INTO" <+> text tbl <+> parens colDocs $+$
     text "VALUES" $+$
     lparen $+$
     myNest 1 (commaAndLineSep exprs) $+$
     rparen

instance SQL_IDENTIFIER columnType => RENDER_SQL (SqlInsert columnType) where
  renderSql = renderDoc


-------------------------------------------------------------------------------
-- - UPDATE -
-------------------------------------------------------------------------------


instance SQL_IDENTIFIER fieldType => DOC (SqlUpdate fieldType) where
  docOf (SqlUpdate tbl cols mbWhereExpr) =
    let
      updateDoc = text "UPDATE" <+> text tbl $+$ text "SET"
      assignmentsDoc = commaAndLineSepDocs $ map colAssignment (NonEmpty.toList cols)
      colAssignment (col,expr) = text (sqlIdentifier col) <+> equals <+> (docOf expr)
      whereDoc    = case mbWhereExpr of
                        Nothing    -> empty
                        Just expr  -> text "WHERE" $+$ myNest 1 (docOf expr)
    in
     updateDoc $+$
     myNest 1 assignmentsDoc $+$
     whereDoc

instance SQL_IDENTIFIER columnType => RENDER_SQL (SqlUpdate columnType) where
  renderSql = renderDoc


-------------------------------------------------------------------------------
-- - DELETE -
-------------------------------------------------------------------------------


instance SQL_IDENTIFIER fieldType => DOC (SqlDelete fieldType) where
  docOf (SqlDelete tbl mbWhereExpr) =
    let
      deleteDoc = text "DELETE FROM" <+> text tbl
      whereDoc  = case mbWhereExpr of
                        Nothing    -> empty
                        Just expr  -> text "WHERE" $+$ myNest 1 (docOf expr)
    in
     deleteDoc $+$
     whereDoc

instance SQL_IDENTIFIER columnType => RENDER_SQL (SqlDelete columnType) where
  renderSql = renderDoc

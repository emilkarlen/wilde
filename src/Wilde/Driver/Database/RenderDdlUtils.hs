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

module Wilde.Driver.Database.RenderDdlUtils
       (
         module Wilde.Database.BackEndDdl,

         renderDdl,
         
         ErrorMessage,
         ColTypeTranslator,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Prelude hiding ((<>))

import Data.List

import Control.Monad.Reader


import Text.PrettyPrint

import Database.HDBC.ColTypes as HDBC

import Wilde.Utils.Utils

import qualified Wilde.Utils.NonEmptyList as NonEmpty

import Wilde.Database.BackEndDdl


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Translates HDBC column info 'SqlColDesc' to the string to
-- use in the rendered SQL.
--
-- | If an error is detected, just 'fail'!
type ColTypeTranslator = HDBC.SqlColDesc -> Either ErrorMessage String


type RenderMonad a = Reader ColTypeTranslator a

getColTypeTranslator :: RenderMonad ColTypeTranslator
getColTypeTranslator = ask

type ErrorMessage = String


renderDdl :: ColTypeTranslator
             -> [PrecededByCommentLines TranslatedDdlStatement]
             -> String
renderDdl colTypeTranslator statements = Text.PrettyPrint.render doc
  where
    doc = runReader (renderDdlStatements statements) colTypeTranslator

renderDdlStatements :: [PrecededByCommentLines TranslatedDdlStatement]
                       -> RenderMonad Doc
renderDdlStatements statements =
  do
    stmtDocs <- mapM renderStmt' statements
    return $ vcat stmtDocs
  where
    renderStmt' = renderPrecededByCommentLinesM renderDdlStatement

renderDdlStatement :: TranslatedDdlStatement -> RenderMonad Doc
renderDdlStatement (CreateTable backEndTableInfo) = renderCreateTable backEndTableInfo
renderDdlStatement (AlterTable  alterInfo)        = renderAlterTable  alterInfo

renderCreateTables :: [PrecededByCommentLines (BackEndTableInfo BackEndColumnInfo)]
                      -> RenderMonad Doc
renderCreateTables statements =
  do
    stmtDocs <- mapM renderTable' statements
    return $ vcat stmtDocs
  where
    renderTable' = renderPrecededByCommentLinesM renderCreateTable

renderPrecededByCommentLinesM :: (a -> RenderMonad Doc)
                                 -> PrecededByCommentLines a
                                 -> RenderMonad Doc
renderPrecededByCommentLinesM renderValue x =
  do
    valueDoc <- renderValue (commentedValue x)
    return $ vcat [commentsDoc,valueDoc]
  where
    commentsDoc :: Doc
    commentsDoc = vcat . map renderCommentLine $ commentLines x

    renderCommentLine line = text "-- " <> text line
      
renderAlterTable :: (SqlIdentifier,[AlterSpecification BackEndColumnInfo]) -> RenderMonad Doc
renderAlterTable (_,[]) = return empty
renderAlterTable (tableName,specifications) =
  do
    specificationDocs <- mapM renderAlterTableSpecification specifications
    return $ (vcat $ header : specificationDocs) <> semi
  where
    header = text "ALTER TABLE" <+> text tableName

renderAlterTableSpecification :: AlterSpecification BackEndColumnInfo -> RenderMonad Doc
renderAlterTableSpecification (AddForeignKey backEndForeignKeyInfo) = return doc
  where
    doc = text "ADD CONSTRAINT" <+>
          renderForeignKey backEndForeignKeyInfo
  
renderCreateTable :: BackEndTableInfo BackEndColumnInfo -> RenderMonad Doc
renderCreateTable ti =
  do
    columnDocs <- renderColumns (tblColumns ti)
    let clausesDoc = nest 2 $ separate $ columnDocs ++ [primKeyDoc] ++ fkDocs
    return $ vcat [ text "CREATE TABLE" <+> text (tblName ti)
                  , lparen $+$ -- forces new line
                    clausesDoc
                  , rparen <> semi
                  ]
  where
    
    primKeyDoc = text "PRIMARY KEY" <+>
                 parens (hcat (punctuate comma primKeyCols))
    primKeyCols = map text $ NonEmpty.toList $ tblPrimaryKeyColumns ti :: [Doc]
    
    fkDocs     :: [Doc]
    fkDocs      = map renderForeignKey (tblForeignKeys ti)
    
    separate   :: [Doc] -> Doc
    separate   = vcat . intersperse comma

renderForeignKey :: BackEndForeignKeyInfo -> Doc
renderForeignKey fki = text "FOREIGN KEY" <+>
                       parens (text (fkSrcColumn fki)) <+>
                       mkRefDef (fkTarget fki)
  where
    mkRefDef  :: ForeignKeyTarget -> Doc
    mkRefDef fkt = text "REFERENCES" <+>
                   text (fkTargetTable fkt) <+>
                   parens (text (fkTargetColumn fkt))

renderColumns :: [BackEndColumnInfo] -> RenderMonad [Doc]
renderColumns cis = 
  do
    columnInfosAsStringLists <- mapM renderColumn cis
    let lines = table ' ' " " columnInfosAsStringLists :: [String]
    return $ map text lines
  -- where
  --   columnInfosAsStringLists = map ci2Strings cis
  --   ci2Strings :: BackEndColumnInfo -> [String]
  --   ci2Strings (BackEndColumnInfo name typ extras) = [name,typ] ++ extras

renderColumn :: BackEndColumnInfo -> RenderMonad [String]
renderColumn (BackEndColumnInfo name typ extras) =
  do
    colTypeTranslator <- getColTypeTranslator
    case colTypeTranslator typ of
      Right typeString -> return $ [name,typeString] ++ extras
      Left  errMsg     -> error errMsg

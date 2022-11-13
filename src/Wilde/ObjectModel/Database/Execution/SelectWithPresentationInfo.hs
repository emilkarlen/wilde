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
-- | Utilities for database IO, handling 'Object's with presentation information.
-------------------------------------------------------------------------------
module Wilde.ObjectModel.Database.Execution.SelectWithPresentationInfo
       (
         inputSelection,
         inputAll,
         inputOne,
         inputOneMandatory,
         input,
         inputForConvertibleParams,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Utils.Utils
import qualified Wilde.Utils.NonEmptyList as NonEmpty

import Database.HDBC

import qualified Wilde.Database.SqlJoin as Sql
import qualified Wilde.Media.Database.Exec as SqlExec
import qualified Wilde.Media.Database.Monad as DbConn

import qualified Wilde.ObjectModel.Database.JoinUtils as OmDbJ
import Wilde.ObjectModel.DatabaseAndPresentation
import Wilde.ObjectModel.ObjectModelUtils

import Wilde.Media.Database

import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.Database.InputExistingSansPresentationInfo as InputExisting

import qualified Wilde.ObjectModel.Database.Sql.WithPresentationInfo as SqlGen


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Data for inputing an 'Attribute' from SQL data.
newtype AttributeTypeDbInputData atConf dbTable e c =
  AttributeTypeDbInputData ((AttributeType atConf dbTable e c,[SqlValue]),
                            Maybe (AttributeWithPresentationInfoDbInputer e,
                                   [SqlValue])
                           )


-------------------------------------------------------------------------------
-- - Special inputers -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
inputAll :: (Database.DATABASE_TABLE otConf
            ,Database.INPUT_FOR_EXISTING atConf
            ,ATTRIBUTE_TYPE_INFO atConf
            )
         => ObjectType otConf atConf dbTable otN idAE idAC
         -> [Any (AttributeType atConf dbTable)]
         -> DbConn.Monad [Object otConf atConf dbTable otN idAE idAC]
inputAll ot orderByInDb = input 
                               ot 
                               (SqlGen.otDatabaseOrderBy orderByInDb) 
                               (return Nothing) 
                               []

-------------------------------------------------------------------------------
-- | Inputs 'Object's that satisfy a given WHERE expression.
-------------------------------------------------------------------------------
inputSelection :: (Database.DATABASE_TABLE otConf
                  ,Database.INPUT_FOR_EXISTING atConf
                  ,ATTRIBUTE_TYPE_INFO atConf
                  )
               => ObjectType otConf atConf dbTable otN idAE idAC
               -> [Any (AttributeType atConf dbTable)]
               -- ^ ORDER BY columns
               -> Sql.JoinMonad dbTable (Maybe (Sql.SqlExpr (Sql.BasedOn dbTable)))
               -- ^ WHERE expression that determines which 'Object's are input.
               -> DbConn.Monad [Object otConf atConf dbTable otN idAE idAC]
inputSelection ot orderByInDb getMbWhereExpr =
  input ot (SqlGen.otDatabaseOrderBy orderByInDb) getMbWhereExpr []

-------------------------------------------------------------------------------
inputOneMandatory :: (Database.DATABASE_TABLE otConf
                     ,Database.COLUMNS_AND_IO_FOR_EXISTING atConf
                     ,ATTRIBUTE_TYPE_INFO atConf
                     )
                  => ObjectType otConf atConf dbTable otN idAE idAC
                  -> idAE
                  -> DbConn.Monad (Object otConf atConf dbTable otN idAE idAC)
inputOneMandatory ot pk =
  do
    mbObject <- inputOne ot pk
    case mbObject of
      Nothing -> DbConn.throwErr $ DbNoRows
                 ("InputOneMandatory/" ++ otCrossRefKey ot)
                 (Just (Mismatch 0 1))
      Just o  -> return o

-------------------------------------------------------------------------------
inputOne :: (Database.DATABASE_TABLE otConf
            ,Database.COLUMNS_AND_IO_FOR_EXISTING atConf
            ,ATTRIBUTE_TYPE_INFO atConf
            )
         => ObjectType otConf atConf dbTable otN idAE idAC
         -> idAE
         -> DbConn.Monad (Maybe (Object otConf atConf dbTable otN idAE idAC))
inputOne ot@(ObjectType {}) pk =
  do
    let (getMbWhereExpr,getEqExprParams) = OmDbJ.atExprEqMb idAt
    objects       <- inputForConvertibleParams ot
                     SqlGen.orderByNone getMbWhereExpr
                     (getEqExprParams pk)
    case objects of
      []  -> return Nothing
      [o] -> return (Just o)
      xs  -> DbConn.throwErr $ DbTooManyRows
             ("InputOne/" ++ otCrossRefKey ot)
             (Just (Mismatch (length xs) 1))
  where
    idAt = otIdAttributeType ot


-------------------------------------------------------------------------------
-- - Generic inputer -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Generic inputer
-------------------------------------------------------------------------------
input :: (Database.DATABASE_TABLE otConf,
          ATTRIBUTE_TYPE_INFO atConf
         ,Database.INPUT_FOR_EXISTING atConf
         )
      => ObjectType otConf atConf dbTable otN idAE idAC
      -> Sql.JoinMonad dbTable        [Sql.SqlExpr (Sql.BasedOn dbTable)]
      -- ^ ORDER BY
      -> Sql.JoinMonad dbTable (Maybe (Sql.SqlExpr (Sql.BasedOn dbTable)))
      -- ^ WHERE
      -> [SqlValue]
      -- ^ Parameters
      -> DbConn.Monad [Object otConf atConf dbTable otN idAE idAC]
input ot getOrderByExprs getMbWhereExpr sqlParams =
  inputObjects ot (inputInfoForOt,selectStatement) sqlParams
  where
    (inputInfoForOt,selectStatement) =
      SqlGen.inputInfoAndSelect ot getMbWhereExpr getOrderByExprs
   

-------------------------------------------------------------------------------
-- | A variant of 'input' where the SQL parameters are gotten
-- via a 'ConvertResult' monad.
-------------------------------------------------------------------------------
inputForConvertibleParams :: (Database.DATABASE_TABLE otConf
                             ,Database.INPUT_FOR_EXISTING atConf
                             ,ATTRIBUTE_TYPE_INFO atConf
                             )
                          => ObjectType    otConf atConf dbTable otN idAE idAC
                          -> Sql.JoinMonad dbTable        [Sql.SqlExpr (Sql.BasedOn dbTable)]
                          -> Sql.JoinMonad dbTable (Maybe (Sql.SqlExpr (Sql.BasedOn dbTable)))
                          -> ConvertResult [SqlValue]
                          -> DbConn.Monad [Object otConf atConf dbTable otN idAE idAC]
inputForConvertibleParams ot getOrderByExprs getMbWhereExpr getSqlParams =
   do
     sqlParams <- DbConn.toMonad getSqlParams
     input ot getOrderByExprs getMbWhereExpr sqlParams

-------------------------------------------------------------------------------
-- | Inputs 'Objects' of a given 'ObjectType' for which \"input-info\" is also given.
--
-- The \"input-info\" must have been constructed for the given 'ObjectType'.
-------------------------------------------------------------------------------
inputObjects :: (Database.COLUMN_NAMES atConf
                ,Database.INPUT_FOR_EXISTING atConf
                )
             => ObjectType otConf atConf dbTable otN idAE idAC
             -> ((SqlGen.AttributeTypeDbInputInfo atConf dbTable idAE idAC,
                  [Any (SqlGen.AttributeTypeDbInputInfo atConf dbTable)]),
                 Sql.SqlSelect (Sql.BasedOn dbTable))
             -> [SqlValue]
             -> DbConn.Monad [Object otConf atConf dbTable otN idAE idAC]
inputObjects ot@(ObjectType {}) (inputInfoForOt,selectStatement) sqlParams =
  do
    records <- SqlExec.select_strict selectStatement sqlParams
    DbConn.toMonad $ mapM (inputObject ot inputInfoForOt) records


-------------------------------------------------------------------------------
-- - input -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inputs an 'Object' from a SQL record.
-------------------------------------------------------------------------------
inputObject :: (Database.COLUMN_NAMES atConf
               ,Database.INPUT_FOR_EXISTING atConf
                )
            => ObjectType                otConf atConf dbTable otN idAE idAC
            -> (SqlGen.AttributeTypeDbInputInfo atConf dbTable     idAE idAC,
                [Any (SqlGen.AttributeTypeDbInputInfo atConf dbTable)])
            -> [SqlValue]
            -> TranslationMonad (Object otConf atConf dbTable otN idAE idAC)
inputObject ot (idAtInfo,nonIdAtInfos) record =
 do
   (idAtData,nonIdAtDatas) <- getInputData (idAtInfo,nonIdAtInfos) record
   idA                     <- inputAttribute idAtData
   nonIdAs                 <- mapM (anyValueApplyM inputAttribute) nonIdAtDatas
   return $ conObject ot idA nonIdAs

-------------------------------------------------------------------------------
inputAttribute :: Database.INPUT_FOR_EXISTING atConf
               => AttributeTypeDbInputData atConf dbTable e c
               -> TranslationMonad (Attribute atConf dbTable e c)
inputAttribute (AttributeTypeDbInputData ((at,plainAtValues), Nothing)) =
  InputExisting.inputAttribute at plainAtValues
inputAttribute (AttributeTypeDbInputData ((at@(AttributeType {}),plainAtValues),(Just ( mkPresValueGetter,presValues)))) =
  do
    v               <- InputExisting.inputAttributeValue at plainAtValues
    presValueGetter <- mkPresValueGetter v presValues
    return $
      Attribute
      {
        attrType         = at
      , attrValue        = v
      , attrPresentation = presValueGetter
      }
    
-------------------------------------------------------------------------------
getInputData :: (Database.COLUMN_NAMES atConf
                ,Database.INPUT_FOR_EXISTING atConf
                )
             => (SqlGen.AttributeTypeDbInputInfo atConf dbTable idAE idAC,
                 [Any (SqlGen.AttributeTypeDbInputInfo atConf dbTable)])
             -> [SqlValue]
             -> TranslationMonad (AttributeTypeDbInputData atConf dbTable idAE idAC,
                                  [Any (AttributeTypeDbInputData atConf dbTable)])
getInputData (idAtInfo,nonIdAtInfos) record =
  let
    (idAtData,rest) = splitForAt    record idAtInfo
    nonIdAtDatas    = splitNonIdAts rest   nonIdAtInfos
  in
   return (idAtData,nonIdAtDatas)
  where
    splitNonIdAts :: Database.COLUMN_NAMES atConf
                  => [SqlValue]
                  -> [Any (SqlGen.AttributeTypeDbInputInfo atConf dbTable)]
                  -> [Any (AttributeTypeDbInputData atConf dbTable)]
    splitNonIdAts _ [] = []
    splitNonIdAts rest (info:infos) = let (dat,rest') = splitForAtAny rest info
                                      in  dat : splitNonIdAts rest' infos

-------------------------------------------------------------------------------
splitForAtAny :: Database.COLUMN_NAMES atConf
              => [SqlValue] -- ^ Rest of row: values for all attributes not yet handled.
              ->  Any (SqlGen.AttributeTypeDbInputInfo atConf dbTable)
              -> (Any (AttributeTypeDbInputData atConf dbTable),[SqlValue])
splitForAtAny sqlValues = anyValueApply $ (\(a,b) -> (Any a,b)) . splitForAt sqlValues

-------------------------------------------------------------------------------
splitForAt :: Database.COLUMN_NAMES atConf
           => [SqlValue] -- ^ Rest of row: values for all attributes not yet handled.
           -> SqlGen.AttributeTypeDbInputInfo  atConf dbTable e c -- ^ Attribute to handle.
           -> (AttributeTypeDbInputData atConf dbTable e c,[SqlValue])
splitForAt rowPart (SqlGen.AttributeTypeDbInputInfo at Nothing) =
  (AttributeTypeDbInputData ((at,plainAtVals),Nothing),rest)
  where
    (plainAtVals,rest) = splitAt numColsForPlainAt rowPart
    numColsForPlainAt  = NonEmpty.length $ Database.atColumns at

splitForAt rowPart (SqlGen.AttributeTypeDbInputInfo at (Just (presExprs,mkA))) =
  (AttributeTypeDbInputData ((at,plainAtVals),Just (mkA,presVals)),rest)
  where
    (plainAtVals,rest1) = splitAt numColsForPlainAt rowPart
    (presVals   ,rest ) = splitAt numColsForPres    rest1
    numColsForPlainAt   = NonEmpty.length $ Database.atColumns at
    numColsForPres      = length presExprs

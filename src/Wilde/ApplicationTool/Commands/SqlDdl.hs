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
-- | Commands for the Application Tool for generating SQL DDL.
-------------------------------------------------------------------------------
module Wilde.ApplicationTool.Commands.SqlDdl
       (
         printCreateTables,

         mkDdlStatements,

         BackEndTableInfo(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------



import Data.Maybe

import qualified Wilde.Utils.NonEmptyList as NonEmpty

import Wilde.Database.Sql
import Wilde.Database.SqlDdlInfo
import Wilde.Database.BackEndDdl

import Wilde.Media.Database

import Wilde.ObjectModel.ObjectModel
import Wilde.ObjectModel.Database as Database
import Wilde.ObjectModel.ObjectModelUtils
import qualified Wilde.ObjectModel.Database as DatabaseClasses (DDL(..))

import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.ObjectTypeWithAtDdlInformation as ObjectTypeWithAtDdlInformation

import Wilde.ApplicationTool.ApplicationModel
import Wilde.ApplicationTool.Command
import Wilde.ApplicationTool.Commands.OrderCreateTables


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


printCreateTables :: OBJECT_TYPES_WITH_DDL_INFO om
                  => CommandWithParsedArgs om
printCreateTables (CommandEnv {
                      objectModel = om,
                      ddlRenderer = renderer
                      }) _ =
  putStrLn (render renderer tableInfos)
  where
    tableInfos :: [PrecededByCommentLines (DdlStatement BackEndColumnInfo)]
    tableInfos  = map (fmap (fmap mkBackEndColumnInfo)) ddlStmts

    ddlStmts   :: [PrecededByCommentLines (DdlStatement (DdlColumnInfo SqlIdentifier))]
    ddlStmts    = mkDdlStatements ots

    ots        = objectTypesWithDdlInfo om

mkDdlStatements :: [ObjectTypeWithAtDdlInformation.AnyO ObjectType]
                -> [PrecededByCommentLines (DdlStatement (DdlColumnInfo SqlIdentifier))]
mkDdlStatements = concatMap create . orderObjectTypesSoThatReferencesAreCreatedAfterTargets

create :: SCC (ObjectTypeWithAtDdlInformation.AnyO ObjectType)
          -> [PrecededByCommentLines (DdlStatement (DdlColumnInfo SqlIdentifier))]
create (AcyclicSCC (ObjectTypeWithAtDdlInformation.AnyO ot@(ObjectType {}))) =
  map (withoutComments . CreateTable) [createTable ot]
create (CyclicSCC aots) = map (withComments comment) ddlStmts
  where
    ddlStmts = ddlCreateTables ++ ddlAlterTablesToAddFKs

    ddlCreateTables = map CreateTable tablesWithoutFKs

    ddlAlterTablesToAddFKs = map (AlterTable . mkAlterTable) tablesWithoutFKs_and_FKs

    tablesWithoutFKs = map fst tablesWithoutFKs_and_FKs

    tablesWithoutFKs_and_FKs :: [(BackEndTableInfo (DdlColumnInfo SqlIdentifier),
                                  [BackEndForeignKeyInfo])]
    tablesWithoutFKs_and_FKs = map popForeignKeys tablesWithFKs

    tablesWithFKs :: [BackEndTableInfo (DdlColumnInfo SqlIdentifier)]
    tablesWithFKs = map (ObjectTypeWithAtDdlInformation.anyODdlApply createTable) aots

    mkAlterTable :: (BackEndTableInfo (DdlColumnInfo SqlIdentifier),
                     [BackEndForeignKeyInfo])
                    -> (SqlIdentifier,[AlterSpecification (DdlColumnInfo SqlIdentifier)])
    mkAlterTable (ti,fkInfos) = (tblName ti,map AddForeignKey fkInfos)

    comment = "Part of group with cyclic references:" :
              map ("   "++) group_members
    group_members = map (ObjectTypeWithAtDdlInformation.anyODdlApply getTableName) $ aots

popForeignKeys :: BackEndTableInfo a -> (BackEndTableInfo a,[BackEndForeignKeyInfo])
popForeignKeys ti = (ti',fks)
  where
    ti' = ti { tblForeignKeys = [] }
    fks = tblForeignKeys ti

getTableName :: Database.DATABASE_TABLE otConf
             => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
             -> String
getTableName = tableName . Database.otDatabaseTable

createTable :: (Database.DATABASE_TABLE otConf
               ,DatabaseClasses.DDL atConf)
            => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
            -> BackEndTableInfo (DdlColumnInfo SqlIdentifier)
createTable ot@(ObjectType {}) =
  BackEndTableInfo
  {
    tblName              = getTableName ot
  , tblColumns           = concatMap fst colsAndFks
  , tblPrimaryKeyColumns = getColNames $ otIdAttributeType ot
  , tblForeignKeys       = concatMap snd colsAndFks
  }
  where
    colsAndFks    :: [([DdlColumnInfo SqlIdentifier],[BackEndForeignKeyInfo])]
    colsAndFks     = map (anyValueApply getColumnInfos) ats
    
    getColNames    = fmap (sqlIdentifier . columnIdent) . DatabaseClasses.atDdlInfo
    getColumnInfos = columnInfos  errMsgHeader'
    ats            = otAttributeTypes ot
    errMsgHeader'  = otCrossRefKey ot

columnInfos :: DatabaseClasses.DDL atConf
            => String
            -> AttributeType atConf dbTable typeForExisting typeForCreate
            -> ([DdlColumnInfo SqlIdentifier],[BackEndForeignKeyInfo])
columnInfos errMsgHeader at = (colInfos,catMaybes mbFkInfos)
  where
    colInfos  :: [DdlColumnInfo SqlIdentifier]
    mbFkInfos :: [Maybe BackEndForeignKeyInfo]
    (colInfos,mbFkInfos) = unzip colInfoList

    colInfoList :: [(DdlColumnInfo SqlIdentifier,Maybe BackEndForeignKeyInfo)]
    colInfoList = getColInfoList at

    getColInfoList :: DatabaseClasses.DDL atConf
                   => AttributeType atConf dbTable typeForExisting typeForCreate
                   -> [(DdlColumnInfo SqlIdentifier,Maybe BackEndForeignKeyInfo)]
    getColInfoList = NonEmpty.toList .
                     fmap (columnInfo errMsgHeader') .
                     DatabaseClasses.atDdlInfo
    errMsgHeader' = errMsgHeader ++ "/" ++ atCrossRefKey at

columnInfo :: String
           -> (DdlColumnInfo dbTable)
           -> (DdlColumnInfo SqlIdentifier,Maybe BackEndForeignKeyInfo)
columnInfo _ ddlCi@(DdlColumnInfo { columnIdent = theColumnIdent }) = (ddlCi',mbFk)
  where
    ddlCi' = setColumnIdent theColName ddlCi :: DdlColumnInfo SqlIdentifier
    theColName = sqlIdentifier theColumnIdent :: SqlIdentifier
    mbFk = fmap
           (\fkt -> BackEndForeignKeyInfo { fkSrcColumn = theColName, fkTarget = fkt })
           (foreignKey ddlCi)

mkBackEndColumnInfo :: DdlColumnInfo SqlIdentifier
                    -> BackEndColumnInfo
mkBackEndColumnInfo ddlCi =
  BackEndColumnInfo
  {
    colName  = columnIdent ddlCi
  , colType  = hdbcColDesc ddlCi
  , colExtra = extraIfNotForeignKey ++ columnExtra ddlCi
  }
  where
    extraIfNotForeignKey :: [String]
    extraIfNotForeignKey = if isForeignKey ddlCi
                           then []
                           else extraWhenNotForeignKey ddlCi


-------------------------------------------------------------------------------
-- - utils -
-------------------------------------------------------------------------------


-- | Short cut to get the columns of an 'AttributeType'.
getAtCols :: DatabaseClasses.DDL atConf
          => AttributeType atConf dbTable typeForExisting typeForCreate
          -> NonEmpty.List (DdlColumnInfo dbTable)
getAtCols = DatabaseClasses.atDdlInfo

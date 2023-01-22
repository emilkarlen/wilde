{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
-- | Utilities related to MySQL for constructing an applications Object Model.
-------------------------------------------------------------------------------
module Wilde.Driver.Database.MySQL.ApplicationObjectModelTools
       (
         ot_PrimaryKey_dbAutogen_MySql,

         IdNameOtConfiguration(..),

         mkConfiguration,

         ot_IdName_dbAutogen_MySql,
         ots_IdName_dbAutogen_MySql,
         ots_and_rps_IdName_dbAutogen_MySql,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Control.Monad.IO.Class

import Database.HDBC
import qualified Database.HDBC as HDBC

import qualified Wilde.Utils.Logging.Monad as Logging

import Wilde.Media.Database
import qualified Wilde.Media.Database.Monad as DbConn

import           Wilde.Database.Sql
import           Wilde.Database.SqlDdlInfo

import qualified Wilde.ObjectModel.ObjectModelUtils as OmUtils
import qualified Wilde.ObjectModel.Database as Database

import Wilde.ApplicationConstruction.ObjectModel.ObjectType
import qualified Wilde.ApplicationConstruction.ObjectModel.IdNameObjectType as IdNameOt
import Wilde.ApplicationConstruction.StandardServices as StandardServices

import Wilde.ApplicationConstruction.Database.DatabaseUtils

import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.DdlAtAnnotation as DdlAtAnnotation
import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.UiIoAndDbIo as UiIoAndDbIo
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.Database as OtDbConfig


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Constructs an 'ObjectType' for a MySql database,
-- who's PK is a 'PrimaryKeyType' that is automatically generated by
-- the MySql AUTO_INC clause.
--
-- Uses the name of the database table as key string ('CrossRefIdentifier').
-------------------------------------------------------------------------------
ot_PrimaryKey_dbAutogen_MySql :: DatabaseTable
                              -> StdAutoPkObjectTypeToNative dbTable otNative
                              -> StdAutoPkPkAttributeType_ddl dbTable -- ^ PK attribute
                              -> [Any (AttributeType DdlAtAnnotation.Configuration dbTable)]
                              -- ^ Non-PK attributes
                              ->StdAutoPkObjectType_ddl dbTable otNative
ot_PrimaryKey_dbAutogen_MySql databaseTable toNative atPk@(AttributeType {}) nonPkAts =
  ObjectType
  {
    otCrossRefKey         = tableName databaseTable
  , otIdAttributeType     = setAutoincOnDdlColInfo atPk
  , otNonIdAttributeTypes = nonPkAts
  , otToNative            = toNative
  , otConfiguration =
    OtDbConfig.Configuration
    {
      OtDbConfig.databaseTable               = databaseTable
    , OtDbConfig.getIdOfInsertedIntoDatabase = getAutoIncValue
    }
  }


-- | Configuration for a ID-name Object Type using MySQL auto-inc PK.
data IdNameOtConfiguration dbTable =
  IdNameOtConfiguration
  {
    objectTypeTitle :: StyledTitle
  , nameTitle       :: Title
  , maxSize         :: Int
  , inputWidth      :: Int
  , dbTableName     :: SqlIdentifier
  , colPk           :: dbTable
  , colName         :: dbTable
  , dbIo_string     :: DatabaseIo String
  }

mkConfiguration :: IdNameOtConfiguration dbTable
                -> IdNameOt.Configuration dbTable
mkConfiguration config =
  IdNameOt.Configuration
  {
    IdNameOt.objectTypeTitle = objectTypeTitle config
  , IdNameOt.nameTitle       = nameTitle config
  , IdNameOt.maxSize         = maxSize config
  , IdNameOt.inputWidth      = inputWidth config
  , IdNameOt.dbTableName     = dbTableName config
  , IdNameOt.colPk           = colPk config
  , IdNameOt.colName         = colName config
  , IdNameOt.dbIo_string     = dbIo_string config
  , IdNameOt.getIdOfInserted = getAutoIncValue
  }

-- | A variant of 'ApplicationObjectModelTools.ot_IdName' with
-- PK generated via MySql:s AUTO INCREMENT.
ot_IdName_dbAutogen_MySql :: SQL_IDENTIFIER dbTable
                          => IdNameOtConfiguration dbTable
                          -> IdNameOt.IdNameObjectType_ddl dbTable
ot_IdName_dbAutogen_MySql config = StandardServices.objectType
                                   (ots_IdName_dbAutogen_MySql config)


-- | A variant of 'ApplicationObjectModelTools.ot_IdName' with
-- PK generated via MySql:s AUTO INCREMENT.
ots_IdName_dbAutogen_MySql :: SQL_IDENTIFIER dbTable
                           => IdNameOtConfiguration dbTable
                           -> IdNameOt.IdNameObjectTypeSetup_ddl dbTable
ots_IdName_dbAutogen_MySql config = snd $ ots_and_rps_IdName_dbAutogen_MySql config

-- | A variant of 'ApplicationObjectModelTools.ot_IdName' with
-- PK generated via MySql:s AUTO INCREMENT.
ots_and_rps_IdName_dbAutogen_MySql :: SQL_IDENTIFIER dbTable
                                   => IdNameOtConfiguration dbTable
                                   -> (IdNameOt.IdNamePresStrSpec_ddl     dbTable,
                                       IdNameOt.IdNameObjectTypeSetup_ddl dbTable)
ots_and_rps_IdName_dbAutogen_MySql config =
  (rps,otsWithAutoincIdAt)
  where
    otsWithAutoincIdAt = ots { StandardServices.objectType = otWithAutoincIdAt }
    otWithAutoincIdAt  = ot { otIdAttributeType = atIdWithAutoinc }
    ot                 = StandardServices.objectType ots
    atIdWithAutoinc    = setAutoincOnDdlColInfo (otIdAttributeType ot)
    (rps,ots)          = IdNameOt.ots_and_rps_IdName
                         (mkConfiguration config)

setAutoincOnDdlColInfo :: AttributeType DdlAtAnnotation.Configuration dbTable e c
                       -> AttributeType DdlAtAnnotation.Configuration dbTable e c
setAutoincOnDdlColInfo = OmUtils.atTransformConfiguration $
                         UiIoAndDbIo.transformAnnotation
                         (DdlAtAnnotation.transformDdlAtAnnotationColumnInfos setAutoinc)

setAutoinc :: DdlColumnInfo dbTable -> DdlColumnInfo dbTable
setAutoinc ddlCi =
  if autoIncStr `elem` extraWhenNotForeignKey ddlCi
  then ddlCi
  else addExtraWhenNotForeignKey [autoIncStr] ddlCi
  where
    autoIncStr = "AUTO_INCREMENT"


-------------------------------------------------------------------------------
-- | The statement to get the value last inserted into an auto-inc column.
-------------------------------------------------------------------------------
selectLastInsertedId = "SELECT LAST_INSERT_ID()"

-------------------------------------------------------------------------------
-- | Function that reads the last value inserted into a AUTO_INCREMENT column
-- in a MySql database.
--
-- Use this method as a 'GetIdOfInsertedIntoDatabase' for an 'ObjectType' who's
-- ID-attribute use MySql AUTO_INCREMENT.
-------------------------------------------------------------------------------
getAutoIncValue :: (Integral idAtExisting
                   ,Convertible SqlValue idAtExisting)
                => Database.GetIdOfInsertedIntoDatabase idAtExisting idAtCreate
getAutoIncValue _ _ = getAutoIncValue'

-------------------------------------------------------------------------------
-- | A variant of 'getAutoIncValue' where the ID may not need to be
-- auto generated if it was supplied in the 'ObjectForCrate'.
-------------------------------------------------------------------------------
getAutoIncValue_optional :: (Integral idAtExisting,
                             Convertible SqlValue idAtExisting)
                         => Database.GetIdOfInsertedIntoDatabase idAtExisting (Maybe idAtExisting)
getAutoIncValue_optional mbValue _ = maybe getAutoIncValue' pure mbValue

-- Helper for getting an AUTO_INCREMENT:ed value.
getAutoIncValue' :: (Integral a,
                     Convertible SqlValue a)
                 => DbConn.Monad a
getAutoIncValue' = Logging.loggBeginEnd Logging.LIBRARY "getAutoIncValue'" $ do
  stmt <- DbConn.prepareSql_str selectLastInsertedId
  Logging.logg Logging.LIBRARY "getAutoIncValue'/HDBC.execute ..."
  liftIO $ HDBC.execute stmt []
  Logging.logg Logging.LIBRARY "getAutoIncValue'/HDBC.fetchAllRows' ..."
  res <- liftIO $ HDBC.fetchAllRows' stmt
  Logging.logg Logging.LIBRARY "getAutoIncValue'/HDBC.convertOneRowOneValue ..."
  retVal <- convertOneRowOneValue safeFromSql errMsg res
  Logging.logg Logging.LIBRARY "getAutoIncValue'/HDBC.convertOneRowOneValue done"
  pure retVal
  where
    errMsg = "Getting generated PK via MySQLs AUTO_INCREMENT: " ++ selectLastInsertedId

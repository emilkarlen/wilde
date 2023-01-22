{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}

-------------------------------------------------------------------------------
-- | Tools for constructing Object Types that consist solely of ID and Name
-- attributes.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.ObjectModel.IdNameObjectType
       (
         -- * Re-exporting common closely related modules

         -- module Wilde.ApplicationConstruction.ObjectModel.AttributeType,
         -- module Wilde.ApplicationConstruction.ObjectModel.ObjectType,

        -- * Complete configuration for the obect type

         Configuration(..),

        -- * Types for the db table and native objects

         Table(..),
         IdNameNative(..),

        -- * Type synonyms

         IdNameType,
         IdNameObjectType,
         IdNameObjectTypeSetup,
         IdNamePresStrSpec,

         IdNameObjectType_ddl,
         IdNameObjectTypeSetup_ddl,
         IdNamePresStrSpec_ddl,

        -- * Variants of the object type setup

         ot_IdName,
         ot_IdName_andNameAt,
         ots_and_rps_IdName,
         ots_IdName,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Database.Sql

import           Wilde.Media.Database

import Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.Database as Database
import Wilde.ObjectModel.ObjectModelUtils

import Wilde.ApplicationConstruction.StandardServices as StandardServices

import Wilde.ApplicationConstruction.StandardServices.Tools
import qualified Wilde.ApplicationConstruction.UserInteraction.Output.ObjectListSetup as OLS
import           Wilde.ApplicationConstruction.ObjectModel.StandardPrimaryKey
import           Wilde.ApplicationConstruction.ObjectModel.ReferenceAttributeType

import Wilde.ApplicationConstruction.ObjectModel.AttributeType

import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.DdlAtAnnotation as DdlAtAnnotation
import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.UiIoAndDbIo as UiIoAndDbIo
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.Database as OtDbConfig

import Wilde.ApplicationConstruction.ObjectModel.ObjectType


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - ObjectType constructors -
-------------------------------------------------------------------------------


type IdNameO t otConf atConf dbTable = StdAutoPkO t otConf atConf dbTable IdNameType

type IdNameO_woAtAnn t dbTable = StdAutoPkO t EmptyOtConfiguration                         UiIoAndDbIo.ConfigurationSansAnnotation dbTable IdNameType
type IdNameO_ddl     t dbTable = StdAutoPkO t OtDbConfig.Configuration DdlAtAnnotation.Configuration           dbTable IdNameType

-- | This type should replace 'IdNameType', I believe.
data IdNameNative =
  IdNameNative
  {
    idValue   :: PrimaryKeyType
  , nameValue :: String
  }

type IdNameType              = (PrimaryKeyType,String)

type IdNameObjectType      dbTable = IdNameO_woAtAnn ObjectType      dbTable
type IdNameObjectTypeSetup dbTable = IdNameO_woAtAnn ObjectTypeSetup dbTable
type IdNamePresStrSpec     dbTable = ReferencePresentationSpec EmptyOtConfiguration UiIoAndDbIo.ConfigurationSansAnnotation dbTable IdNameType PrimaryKeyType (Maybe PrimaryKeyType)

type IdNameObjectType_ddl      dbTable = IdNameO_ddl ObjectType      dbTable
type IdNameObjectTypeSetup_ddl dbTable = IdNameO_ddl ObjectTypeSetup dbTable
type IdNamePresStrSpec_ddl     dbTable = ReferencePresentationSpec OtDbConfig.Configuration DdlAtAnnotation.Configuration dbTable IdNameType PrimaryKeyType (Maybe PrimaryKeyType)

-- | Table/Column type.
data Table = Id
           | Name
           deriving Show

instance SQL_IDENTIFIER Table where
  sqlIdentifier Id   = "id"
  sqlIdentifier Name = "name"


-- | All information needed to construct an Object Type
-- for ID and Name objects.
data Configuration dbTable =
  Configuration
  {
    objectTypeTitle :: StyledTitle
  , nameTitle       :: Title
  , maxSize         :: Int
  , inputWidth      :: Int
  , dbTableName     :: SqlIdentifier
  , colPk           :: dbTable
  , colName         :: dbTable
  , dbIo_string     :: DatabaseIo String
  , getIdOfInserted :: Database.GetIdOfInsertedIntoDatabase PrimaryKeyType (Maybe PrimaryKeyType)
  }

-- | Gives an name-with-ID 'ObjectType'.
--
-- This is an 'ObjectType' with only two 'AttributeType's: a PK
-- and a String that is a \"name\".
ot_IdName :: (SQL_IDENTIFIER dbTable)
          => Configuration dbTable
          -> IdNameObjectType_ddl dbTable
ot_IdName config = fst $ ot_IdName_andNameAt config

-- | Gives an name-with-ID 'ObjectType'.
--
-- This is an 'ObjectType' with only two 'AttributeType's: a PK
-- and a String that is a \"name\".
ots_IdName :: SQL_IDENTIFIER dbTable
           => Configuration dbTable
           -> IdNameObjectTypeSetup_ddl dbTable
ots_IdName config = snd $ ots_and_rps_IdName config

-- | Gives an name-with-ID 'ObjectType'.
--
-- This is an 'ObjectType' with only two 'AttributeType's: a PK
-- and a String that is a \"name\".
ots_and_rps_IdName :: (SQL_IDENTIFIER dbTable)
                   => Configuration dbTable
                   -> (IdNamePresStrSpec_ddl dbTable,
                       IdNameObjectTypeSetup_ddl dbTable)
ots_and_rps_IdName config =
  (
    refPresSpec_4_stringAt atName,
    objectTypeSetup ot (objectTypeTitle config)
    `withObjectListDisplaySetup`
    OLS.ObjectListDisplaySetup [anyAtName] [anyAtName] (return Nothing)
   )
  where
    anyAtName   = Any atName
    (ot,atName) = ot_IdName_andNameAt config

-- | Gives an name-with-ID 'ObjectType'.
--
-- This is an 'ObjectType' with only two 'AttributeType's: a PK
-- and a String that is a \"name\".
ot_IdName_andNameAt :: SQL_IDENTIFIER dbTable
                    => Configuration dbTable
                    -> (IdNameObjectType_ddl dbTable,
                        AttributeType DdlAtAnnotation.Configuration dbTable String String)
ot_IdName_andNameAt config =
  (ot,atName)
  where
    ot = ObjectType
         {
           otCrossRefKey             = dbTableName config
         , otIdAttributeType         = atPk
         , otNonIdAttributeTypes     = [atAvName]
         , otToNative                = ObjectToNativeFunction toNative
         , otConfiguration =
           OtDbConfig.Configuration
           {
             OtDbConfig.databaseTable               = DatabaseTable (dbTableName config)
           , OtDbConfig.getIdOfInsertedIntoDatabase = getIdOfInserted config
           }
         }
    atPk     = at_PrimaryKey_dbAutogen 5 (colPk config)
    atName   = at_String
               (dbIo_string config)
               (maxSize config)
               (inputWidth config)
               (colName config)
               noDefault
               (nameTitle config)
    atAvName = Any atName
    toNative :: Object otConf atConf dbTable (PrimaryKeyType,String) PrimaryKeyType idAtCreate
             -> ObjectToNativeResult (PrimaryKeyType,String)
    toNative o =
      let
        pk = attrValue $ oIdAttribute o
      in
       case oNonIdAttributes o of
         [Any aName@(Attribute {})] ->
           do
             name <- doOtnUnhide $ attrValue aName
             return (pk,name)
         attrs -> numAttributesError2 attrs 1

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

-- | Test data that is shared by many tests.
module TestResources.TestData
       (
         noDbIo,
         
         elementKeyPrefixes,

         -- * A Test Object Type

         PrimaryKeyType,
         PkNameTable(..),

         PkNameObjectType,
         PkNameObject,

         otsPkName,
         otPkName,
         rpsPkName,
         at_pk,
         at_name,
         dbPkNameTable,

         -- * Objects of the Test Object Type

         oPkName1,
         oPkName2,

         -- * Input field width

         inputWidth,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Database.Sql

import Wilde.WildeUi.StdValueTypes

import Wilde.Media.Database
import Wilde.Media.UserInteraction

import Wilde.ObjectModel.ObjectModelUtils

import           Wilde.ApplicationConstruction.ObjectModel.ObjectType
import qualified Wilde.ApplicationConstruction.StandardServices as StandardServices
import           Wilde.ApplicationConstruction.StandardServices.Tools
import           Wilde.ApplicationConstruction.ObjectModel.ReferenceAttributeType

import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.DdlAtAnnotation as DdlAtAnnotation
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.Database as OtDbConfig

import qualified Wilde.Media.Database as DB


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


noDbIo :: DB.DatabaseIo a
noDbIo = error "Database IO should not be used"


-------------------------------------------------------------------------------
-- | A list of all kinds of 'ElementKeyPrefixes'.
-------------------------------------------------------------------------------
elementKeyPrefixes :: [(String,ElementKeyPrefix)]
elementKeyPrefixes =
  [ ("empty"        ,[])
  , ("single short" ,["a"])
  , ("single long"  ,["abcde"])
  , ("multi"        ,["a","b","cde"])
  ]


-------------------------------------------------------------------------------
-- - Object Model -
-------------------------------------------------------------------------------


data PkNameTable = ColumnPk
                 | ColumnName
                 | ColumnNotUsedByAnyAtInOt
                 deriving (Show,Eq)

instance SQL_IDENTIFIER PkNameTable where
  sqlIdentifier = show

type PkName = () -- PrimaryKeyType

type PkNameObjectType = ObjectType OtDbConfig.Configuration DdlAtAnnotation.Configuration PkNameTable PkName PrimaryKeyType PrimaryKeyType
type PkNameObject     = Object     OtDbConfig.Configuration DdlAtAnnotation.Configuration PkNameTable PkName PrimaryKeyType PrimaryKeyType

oPkName1,oPkName2 :: PkNameObject
oPkName1 = mkObject 1 "Number 1"
oPkName2 = mkObject 2 "Number 2"

-- oPkName1,oPkName2 :: PkNameObject
-- oPkName1 = conObject otPkName (atConPlainAttr at_pk 1)
--            [Any $ atConPlainAttr at_name "Number 1"]

-- oPkName2 = conObject otPkName (atConPlainAttr at_pk 2)
--            [Any $ atConPlainAttr at_name "Number 2"]

mkObject :: PrimaryKeyType -> String -> PkNameObject
mkObject id name = conObject otPkName idAttr [Any (nameAttr)]
  where
    idAttr =
      Attribute
      {
        attrType         = at_pk
      , attrValue        = id
      , attrPresentation = return $ AnySVALUE (Word32Value id)
      }
    nameAttr =
      Attribute
      {
        attrType         = at_name
      , attrValue        = name
      , attrPresentation = return $ AnySVALUE (UnquotedStringValue name)
      }

otsPkName :: StandardServices.ObjectTypeSetup
             OtDbConfig.Configuration
             DdlAtAnnotation.Configuration 
             PkNameTable 
             PkName 
             PrimaryKeyType 
             PrimaryKeyType
otsPkName = objectTypeSetup otPkName $
            withNeutralWildeStyle "PKN"

rpsPkName :: ReferencePresentationSpec 
             OtDbConfig.Configuration
             DdlAtAnnotation.Configuration 
             PkNameTable 
             PkName 
             PrimaryKeyType 
             PrimaryKeyType
rpsPkName = refPresSpec_4_stringAt at_name

-- | An 'ObjectType' who's only 'AttributeType' is a standard primary key.
otPkName :: ObjectType OtDbConfig.Configuration DdlAtAnnotation.Configuration PkNameTable PkName PrimaryKeyType PrimaryKeyType
otPkName = ObjectType
           {
             otCrossRefKey             = tableName dbPkNameTable
           , otIdAttributeType         = at_pk
           , otNonIdAttributeTypes     = [Any at_name]
           , otToNative                = ObjectToNativeFunction $ return $ return ()
           , otConfiguration =
             OtDbConfig.Configuration
             {
               OtDbConfig.databaseTable               = dbPkNameTable
             , OtDbConfig.getIdOfInsertedIntoDatabase = getIdOfInsertedWhenEqualToCreate
             }
           }

inputWidth :: Int
inputWidth = 10

at_pk   = at_PrimaryKeyType             inputWidth ColumnPk   noDefault "ID"
at_name = at_String_forDefaultDbIo  100 inputWidth ColumnName noDefault "Name"

dbPkNameTable :: DatabaseTable
dbPkNameTable = DatabaseTable
                  {
                    tableName = "pk_name"
                  }

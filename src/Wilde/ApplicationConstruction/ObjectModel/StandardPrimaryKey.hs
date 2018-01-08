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
-- | Definition of the \"standard\" type of database Primary Key.
--
-- Defines all information needed by 'AttributeType's that use
-- this \"standard\" Primary Key type.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.ObjectModel.StandardPrimaryKey
       (
         -- * The Primary Key type

         PrimaryKeyType,

         -- * presentation

         pres_PrimaryKeyType,
         pres_PrimaryKeyType_optional,

         presO_PrimaryKeyType,
         presO_PrimaryKeyType_optional,
         
         -- * AttributeTypeDatabaseInfo:s
         
         atDbInfo_PrimaryKeyType,
         atDbInfo_PrimaryKeyType_optional,
         atDbInfo_PrimaryKeyType_optionalOnCreate,
         
         -- * AttributeTypeDatabaseConfigForExisting:s
         
         atDbConfE_PrimaryKeyType,
         atDbConfE_PrimaryKeyType_optional,

         -- * DB IO -

         dbIo_PrimaryKeyType,
         dbIo_PrimaryKeyType_optional,
         dbIo_PrimaryKeyType_optionalOnCreate,

         -- * UI IO

         uiIo_PrimaryKeyType ,
         uiIo_PrimaryKeyType_optional ,
         uiIo_PrimaryKeyType_optionalOnCreate ,

         -- * utilities

         atGsrIo_PrimaryKey,
         atGsrIo_PrimaryKey_optionalOnCreate,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Word

import Wilde.Database.Sql (SQL_IDENTIFIER)

import Wilde.ObjectModel.ObjectModel

import qualified Wilde.ApplicationConstruction.Database.AttributeTypeDatabaseInfo as AtDbInfo

import Wilde.ApplicationConstruction.Presentation.Presentation
import Wilde.ApplicationConstruction.GenericStringRepIo
import Wilde.ApplicationConstruction.UserInteraction.Io


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - PrimaryKeyType -
-------------------------------------------------------------------------------


-- | The standard type for primary keys in the database.
type PrimaryKeyType = Word32


-------------------------------------------------------------------------------
-- - presentation -
-------------------------------------------------------------------------------


pres_PrimaryKeyType :: Title -- ^ Title of the attribute type.
                    -> AttributeTypePresentation PrimaryKeyType
pres_PrimaryKeyType = pres_Integral

pres_PrimaryKeyType_optional :: Title -- ^ Title of the attribute type.
                             -> AttributeTypePresentation (Maybe PrimaryKeyType)
pres_PrimaryKeyType_optional = pres_Integral_optional

presO_PrimaryKeyType :: PresentationOutputer PrimaryKeyType
presO_PrimaryKeyType = presO_Integral

presO_PrimaryKeyType_optional :: PresentationOutputer (Maybe PrimaryKeyType)
presO_PrimaryKeyType_optional = presO_Integral_optional


-------------------------------------------------------------------------------
-- - AttributeTypeDatabaseConfigForExisting:s -
-------------------------------------------------------------------------------


atDbConfE_PrimaryKeyType :: (SQL_IDENTIFIER dbTable)
                         => dbTable
                         -> AtDbInfo.AttributeTypeDatabaseConfigForExisting dbTable PrimaryKeyType
atDbConfE_PrimaryKeyType = AtDbInfo.mkAtDbConfigForE . AtDbInfo.word32

atDbConfE_PrimaryKeyType_optional :: (SQL_IDENTIFIER dbTable)
                                  => dbTable
                                  -> AtDbInfo.AttributeTypeDatabaseConfigForExisting dbTable (Maybe PrimaryKeyType)
atDbConfE_PrimaryKeyType_optional = AtDbInfo.mkAtDbConfigForE . AtDbInfo.word32_optional


-------------------------------------------------------------------------------
-- - AttributeTypeDatabaseInfo:s -
-------------------------------------------------------------------------------


atDbInfo_PrimaryKeyType :: (SQL_IDENTIFIER dbTable)
                        => dbTable
                        -> AtDbInfo.AttributeTypeDatabaseInfo_same dbTable PrimaryKeyType
atDbInfo_PrimaryKeyType = AtDbInfo.word32

atDbInfo_PrimaryKeyType_optional :: SQL_IDENTIFIER dbTable
                                 => dbTable
                                 -> AtDbInfo.AttributeTypeDatabaseInfo_same_optional dbTable PrimaryKeyType
atDbInfo_PrimaryKeyType_optional = AtDbInfo.word32_optional

atDbInfo_PrimaryKeyType_optionalOnCreate :: SQL_IDENTIFIER dbTable
                                         => dbTable
                                         -> AtDbInfo.AttributeTypeDatabaseInfo dbTable PrimaryKeyType (Maybe PrimaryKeyType)
atDbInfo_PrimaryKeyType_optionalOnCreate = AtDbInfo.word32_optionalOnCreate


-------------------------------------------------------------------------------
-- - DB IO -
-------------------------------------------------------------------------------


dbIo_PrimaryKeyType :: SQL_IDENTIFIER dbTable
                    => dbTable
                    -> AtDbInfo.AttributeTypeDatabaseInfo_same dbTable PrimaryKeyType
dbIo_PrimaryKeyType = AtDbInfo.word32

dbIo_PrimaryKeyType_optional :: SQL_IDENTIFIER dbTable
                             => dbTable
                             -> AtDbInfo.AttributeTypeDatabaseInfo_same dbTable (Maybe PrimaryKeyType)
dbIo_PrimaryKeyType_optional = AtDbInfo.word32_optional

dbIo_PrimaryKeyType_optionalOnCreate :: SQL_IDENTIFIER dbTable
                                     => dbTable
                                     -> AtDbInfo.AttributeTypeDatabaseInfo dbTable PrimaryKeyType (Maybe PrimaryKeyType)
dbIo_PrimaryKeyType_optionalOnCreate = AtDbInfo.word32_optionalOnCreate


-------------------------------------------------------------------------------
-- - UI IO -
-------------------------------------------------------------------------------


uiIo_PrimaryKeyType  :: Int -- ^ Input width
                     -> AttributeTypeUserInteractionIo PrimaryKeyType PrimaryKeyType
uiIo_PrimaryKeyType = uiIo_Word32

uiIo_PrimaryKeyType_optional  :: Int
                              -> AttributeTypeUserInteractionIo (Maybe PrimaryKeyType) (Maybe PrimaryKeyType)
uiIo_PrimaryKeyType_optional = uiIo_Word32_optional

uiIo_PrimaryKeyType_optionalOnCreate  :: Int -- ^ Input field width.
                                         -> AttributeTypeUserInteractionIo PrimaryKeyType (Maybe PrimaryKeyType)
uiIo_PrimaryKeyType_optionalOnCreate inputWidth =
  uiIo_asString_optionalOnCreate inputWidth True True show readUiiMonad



-------------------------------------------------------------------------------
-- - utilities -
-------------------------------------------------------------------------------


atGsrIo_PrimaryKey :: AttributeGenericStringRepIo PrimaryKeyType PrimaryKeyType
atGsrIo_PrimaryKey = atGsrIo_convertibleFromInteger_nonEmpty

atGsrIo_PrimaryKey_optionalOnCreate :: AttributeGenericStringRepIo PrimaryKeyType (Maybe PrimaryKeyType)
atGsrIo_PrimaryKey_optionalOnCreate =
  atGsrIo_optionalForCreate atGsrIo_PrimaryKey

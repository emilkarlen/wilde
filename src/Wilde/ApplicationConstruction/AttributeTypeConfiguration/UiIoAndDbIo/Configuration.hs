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

{-# LANGUAGE ExistentialQuantification #-}

-------------------------------------------------------------------------------
-- | Declares the Attribute Type Configuration.
--
-- This module should only be used for the implementation of this package.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.AttributeTypeConfiguration.UiIoAndDbIo.Configuration
       (
         Configuration(..),
         AttributeTypeMediaIoForCreate(..),
         AttributeTypeDatabaseConfigForExisting(..),

         transformAnnotation,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Typeable

import qualified Wilde.Database.SqlJoin as Sql

import Wilde.ObjectModel.ObjectModel
import Wilde.ObjectModel.DatabaseAndPresentation
import qualified Wilde.ObjectModel.Database as Database
import Wilde.ObjectModel.UserInteraction

import           Wilde.ApplicationConstruction.GenericStringRepIo
import           Wilde.ApplicationConstruction.Database.AttributeTypeDatabaseInfo (AttributeTypeDatabaseConfigForExisting(..))
import qualified Wilde.ApplicationConstruction.UserInteraction.Io as UiIo


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - the type -
-------------------------------------------------------------------------------


-- | An attribute in an 'ObjectType'.
-- An attribute is either a \"plain\" attribute - just data stored in the table.
-- Or it is a reference to another 'ObjectType' (a \"foreign key\" in the database) .
data Configuration ann dbTable typeForExisting typeForCreate =
  (Sql.SQL_IDENTIFIER dbTable
  ,Typeable typeForExisting,Show typeForExisting) => Configuration
  {
    presentationO            :: AttributeTypePresentation typeForExisting
  , genericStringRepIo       :: AttributeGenericStringRepIo typeForExisting typeForCreate
  , uiIoForAttributeName     :: UiIo.AttributeTypeUiIoForExisting typeForExisting
  , uiCreateOption           :: Maybe (AttributeTypeCreateOption typeForCreate)
  , mediaIoForCreate         :: AttributeTypeMediaIoForCreate typeForExisting typeForCreate
  , databaseConfForExisting  :: AttributeTypeDatabaseConfigForExisting     dbTable typeForExisting
  , dbPresentationInfoGetter :: AttributeWithPresentationInfoDbInputerInfo dbTable typeForExisting
  , annotation               :: ann dbTable typeForExisting typeForCreate
  }


-- | IO for all medias needed for the typeForCreate for an 'AttributeType'.
data AttributeTypeMediaIoForCreate typeForExisting typeForCreate =
  AttributeTypeMediaIoForCreate
  {
    -- | Transforms a value used for creating a new object to the database media.
    --
    -- Gives a list of column values.
    -- Same length as 'atdbioStructure'.
    --
    -- The 'ConnWrapper' is the connection and transaction used for inserting
    -- a new 'Object' into the database.
    aticDatabaseOutputer :: Database.OutputerWithConnection typeForCreate

    -- | Constructs \"components\" for letting the user inputting
    -- an attribute as part of an object to create.
    --
    -- (The object is created by inserting it into the database).
  , aticUiIo :: UiIo.AttributeTypeUiIoForCreate typeForExisting typeForCreate
  }

-- | Transforms the annotation of an object.
transformAnnotation :: (ann dbTable typeForExisting typeForCreate
                        -> ann' dbTable typeForExisting typeForCreate
                        )
                    -> Configuration ann  dbTable typeForExisting typeForCreate
                    -> Configuration ann' dbTable typeForExisting typeForCreate
transformAnnotation f x@(Configuration { annotation = theAnnotation }) =
  x { annotation = f theAnnotation }

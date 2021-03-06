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

-- | Utilities related to database SQL generation.
--
module Wilde.ObjectModel.Database
       (
         -- * Types
         
         OutputerWithConnection,
         GetIdOfInsertedIntoDatabase,
         
         -- * Classes
         
         DATABASE_TABLE(..),
         OBJECT_TYPE_INSERT(..),
         
         COLUMN_NAMES(..),
         
         OUTPUT_FOR_EXISTING(..),
         OUTPUT_FOR_CREATE(..),
         
         INPUT_FOR_EXISTING(..),
         DDL(..),

         IO_FOR_EXISTING(..),
         COLUMNS_AND_IO_FOR_EXISTING(..),

         DATABASE_IO(..),
         DATABASE_IO_AND_DDL(..),
         
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Utils.NonEmptyList as NonEmpty

import Wilde.Media.Database.Monad

import qualified Wilde.Database.Executor as SqlExec
import Wilde.ObjectModel.ObjectModelUtils

import Wilde.Database.SqlDdlInfo


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------
       

-- | Outputer for the database media, for a given value, in a connection.
type OutputerWithConnection a =
  a -> SqlExec.ConnectionAndRenderer -> DatabaseMonad DatabaseOutput

-------------------------------------------------------------------------------
-- | A function that gets the ID/PK of an object that has been
-- created by inserting it into the database.
-- (This method is needed since the ID may be determined by the
-- database system while inserting the database record.
-- E.g. as a MySQL auto_increment column,
-- or from a sequence (Oracle,PostgreSQL).)
--
-- Arguments:
--
-- [@SqlExec.ConnectionAndRenderer@]
-- A connection with the transaction of the insertion.
--
-- [@AttributeForCreate@]
-- The ID AttributeType value in the ObjectForCreate that is inserted.
--
-- [@DatabaseOutput@]
-- The 'SqlValue's that was returned for the ID AttributeForCreate
-------------------------------------------------------------------------------
type GetIdOfInsertedIntoDatabase e c =
  SqlExec.ConnectionAndRenderer
  -> c
  -> DatabaseOutput
  -> DatabaseMonad e

class DATABASE_TABLE otConf where
  otDatabaseTable :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                  -> DatabaseTable

class DATABASE_TABLE otConf => OBJECT_TYPE_INSERT otConf where
  otDatabaseGetIdOfInserted :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                            -> GetIdOfInsertedIntoDatabase idAtExisting idAtCreate
    -- ^ Gets the ID of an 'ObjectForCreate' that has just been inserted into the database.
    -- The reason for having this function is that this ID can be accessed/generated
    -- in different ways, e.g. MySql AUTO_INCREMENT.
    --
    -- TODO Move? Probably - encapsulate with DatabaseStructure into a
    -- all-database-thing.
  

class COLUMN_NAMES atConf where
  atColumns :: AttributeType atConf dbTable typeForExisting typeForCreate
            -> NonEmpty.List (DatabaseColumn dbTable)

class OUTPUT_FOR_CREATE atConf where
  atOutputerForCreate :: AttributeType atConf dbTable typeForExisting typeForCreate
                      -> OutputerWithConnection typeForCreate

class OUTPUT_FOR_EXISTING atConf where
  atOutputerExisting :: AttributeType atConf dbTable typeForExisting typeForCreate
                     -> DatabaseOutputer typeForExisting

class INPUT_FOR_EXISTING atConf where
  atInputerExisting :: AttributeType atConf dbTable typeForExisting typeForCreate
                    -> DatabaseInputer typeForExisting

-- | Information for generating DDL for an 'AttributeType'.
class DDL atConf where
  atDdlInfo :: AttributeType atConf dbTable typeForExisting typeForCreate
            -> NonEmpty.List (DdlColumnInfo dbTable)

-- | Short cut class for input + output for existing.
class (OUTPUT_FOR_EXISTING atConf,INPUT_FOR_EXISTING atConf) => IO_FOR_EXISTING atConf where

-- | Short cut class for input + output for existing.
class (COLUMN_NAMES atConf,IO_FOR_EXISTING atConf) => COLUMNS_AND_IO_FOR_EXISTING atConf where

-- | Short cut class for all database funtionallity.
class (COLUMNS_AND_IO_FOR_EXISTING atConf,OUTPUT_FOR_CREATE atConf) => DATABASE_IO atConf where

-- | Short cut class for database DDL and IO funtionallity.
class (DATABASE_IO atConf,DDL atConf) => DATABASE_IO_AND_DDL atConf where

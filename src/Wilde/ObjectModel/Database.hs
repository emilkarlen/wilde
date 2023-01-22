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


import qualified Data.List.NonEmpty as NonEmpty

import Wilde.Media.Database
import qualified Wilde.Media.Database.Monad as DbConn

import Wilde.ObjectModel.ObjectModelUtils
    ( ObjectType, AttributeType )

import Wilde.Database.SqlDdlInfo


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Outputer for the database media, for a given value, in a connection.
type OutputerWithConnection a = a -> DbConn.Monad DatabaseOutput

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
-- The 'SqlValue's that was pureed for the ID AttributeForCreate
-------------------------------------------------------------------------------
type GetIdOfInsertedIntoDatabase e c =
  c
  -> DatabaseOutput
  -> DbConn.Monad e

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
            -> NonEmpty.NonEmpty (DatabaseColumn dbTable)

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
            -> NonEmpty.NonEmpty (DdlColumnInfo dbTable)

-- | Short cut class for input + output for existing.
class (OUTPUT_FOR_EXISTING atConf,INPUT_FOR_EXISTING atConf) => IO_FOR_EXISTING atConf where

-- | Short cut class for input + output for existing.
class (COLUMN_NAMES atConf,IO_FOR_EXISTING atConf) => COLUMNS_AND_IO_FOR_EXISTING atConf where

-- | Short cut class for all database funtionallity.
class (COLUMNS_AND_IO_FOR_EXISTING atConf,OUTPUT_FOR_CREATE atConf) => DATABASE_IO atConf where

-- | Short cut class for database DDL and IO funtionallity.
class (DATABASE_IO atConf,DDL atConf) => DATABASE_IO_AND_DDL atConf where

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
-- | \"Config\" of database IO for types.
--
-- The structure 'DatabaseColumnType' holds information about a single type.
-- Such a structure is defined here for some common types one might want to
-- in use an application.
--
-- Import this module qualified.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.Database.AttributeTypeDatabaseInfo
       (
         -- * Types

         AttributeTypeDatabaseInfo(..),
         AttributeTypeDatabaseInfoForExisting(..),
         AttributeTypeDatabaseConfigForExisting(..),

         -- ** Synonyms for common variants of 'AttributeTypeDatabaseInfo'

         AttributeTypeDatabaseInfo_same,
         AttributeTypeDatabaseInfo_same_optional,

         -- ** Utilities

         mkAtDbConfigForE,
         atAttributeTypeDatabaseInfo,

         -- * Infor for some types

         -- ** Numbers

         -- *** Int

         int,
         int_optional,

         int32,
         int32_optional,

         int64,
         int64_optional,

         -- *** Word

         word32,
         word32_optional,
         word32_optionalOnCreate,

         word64,
         word64_optional,
         word64_optionalOnCreate,

         -- *** Floating point

         double,
         double_optional,

         -- ** Strings

         -- *** Variable length String with given max size

         string,
         string_optional,

         string_forDbIo,
         string_optional_forDbIo,

         -- *** Variable length String with impl dependent max size

         longString,
         longString_optional,

         longString_forDefaultDbIo,
         longString_optional_forDefaultDbIo,

         -- ** Misc

         -- *** Bool

         bool,
         bool_optional,

         -- *** Date

         day,
         day_optional,

       )
       where

-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Word ( Word32, Word64 )
import Data.Int ( Int32, Int64 )
import Data.Time.Calendar (Day)

import Data.Typeable ( Typeable )

import Database.HDBC.ColTypes ( SqlColDesc )

import qualified Wilde.Utils.NonEmptyList as NonEmpty

import Wilde.Media.Database

import qualified Wilde.ObjectModel.Database as Database

import           Wilde.Database.SqlDdlInfo
import Wilde.Database.Sql ( SQL_IDENTIFIER )

import qualified Wilde.ApplicationConstruction.Database.DatabaseColumnTypes as DbCol


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Database Input and Output functionality of an 'Attribute' and 'AttributeType'.
--
-- Parametrized by:
--
-- [@dbTable@]
-- The type that identifies the table.
--
-- [@typeForExisting@]
-- The type that represents an existing attribute value.
--
-- [@typeForCreate@]
-- The type that represents an attribute value that should be created by inserting
-- it into the database.
--
data AttributeTypeDatabaseInfo dbTable typeForExisting typeForCreate =
  (SQL_IDENTIFIER dbTable
  ,Typeable typeForExisting,Show typeForExisting) =>
  AttributeTypeDatabaseInfo
  {
    -- | Information about values of existing 'Attribute's.
    atdbioInfoForExisting :: AttributeTypeDatabaseInfoForExisting dbTable typeForExisting

    -- | Transforms a value used for creating a new object to the database media.
    --
    -- Gives a list of column values.
    -- Same length as 'atdbioStructure'.
  , atdbioCreateOutputer :: Database.OutputerWithConnection typeForCreate
  }

-- | Config for an 'AttributeType's existing 'Attribute's.
data AttributeTypeDatabaseInfoForExisting dbTable typeForExisting =
  (SQL_IDENTIFIER dbTable
  ,Typeable typeForExisting,Show typeForExisting) =>
  AttributeTypeDatabaseInfoForExisting
  {
    -- | Gives a list of column values.
    -- Same length as 'atdbioStructure'.
    atdbiofeIo :: DatabaseIo typeForExisting

    -- | List of column name and column structure.
    -- Gives one element for each column used.
    -- The length of this list is the number of columns used for both input and output.
    -- Using this information it is possible to generate SQL DDL for the attribute.
  , atdbiofeStructure :: NonEmpty.List (DdlColumnInfo dbTable)
  }

data AttributeTypeDatabaseConfigForExisting dbTable a =
  SQL_IDENTIFIER dbTable =>
  AttributeTypeDatabaseConfigForExisting
  {
    atdbioeIo        :: DatabaseIo a
  , atdbioeStructure :: NonEmpty.List (DatabaseColumn dbTable)
  }

-- | Makes a 'AttributeTypeDatabaseConfigForExisting'.
mkAtDbConfigForE :: AttributeTypeDatabaseInfo              dbTable typeForExisting typeForCreate
                 -> AttributeTypeDatabaseConfigForExisting dbTable typeForExisting
mkAtDbConfigForE (AttributeTypeDatabaseInfo {atdbioInfoForExisting = dbInfoE}) =
  AttributeTypeDatabaseConfigForExisting
  {
    atdbioeIo        = atdbiofeIo dbInfoE
  , atdbioeStructure = (DatabaseColumn . columnIdent) <$> atdbiofeStructure dbInfoE
  }

atAttributeTypeDatabaseInfo :: AttributeTypeDatabaseInfoForExisting dbTable typeForExisting
                            -> Database.OutputerWithConnection                              typeForCreate
                            -> AttributeTypeDatabaseInfo            dbTable typeForExisting typeForCreate
atAttributeTypeDatabaseInfo atdbie@(AttributeTypeDatabaseInfoForExisting {}) dbCreateOutputer =
  AttributeTypeDatabaseInfo
  {
    atdbioInfoForExisting   = atdbie
  , atdbioCreateOutputer = dbCreateOutputer
  }

-------------------------------------------------------------------------------
-- | Synonym for a 'AttributeTypeDatabaseInfo_same' where
-- @typeForExisting@ and @typeForCreate@ are the same.
-------------------------------------------------------------------------------
type AttributeTypeDatabaseInfo_same dbTable a =
  AttributeTypeDatabaseInfo dbTable a a

-------------------------------------------------------------------------------
-- | Synonym for a 'AttributeTypeDatabaseInfo_same' where
-- @typeForExisting@ and @typeForCreate@ are the same
-- and the given value type is optional.
--
-- The given type is wrapped in a 'Maybe'.
-------------------------------------------------------------------------------
type AttributeTypeDatabaseInfo_same_optional dbTable a =
  AttributeTypeDatabaseInfo_same dbTable (Maybe a)


-------------------------------------------------------------------------------
-- - Int -
-------------------------------------------------------------------------------


int :: (SQL_IDENTIFIER dbTable)
    => dbTable
    -> AttributeTypeDatabaseInfo_same dbTable Int
int = atDbIo_forSingleColumn DbCol.int

int_optional :: (SQL_IDENTIFIER dbTable)
             => dbTable
             -> AttributeTypeDatabaseInfo_same_optional dbTable Int
int_optional = atDbIo_forSingleColumn DbCol.int_optional


-------------------------------------------------------------------------------
-- - Int32 -
-------------------------------------------------------------------------------


int32 :: (SQL_IDENTIFIER dbTable)
      => dbTable
      -> AttributeTypeDatabaseInfo_same dbTable Int32
int32 = atDbIo_forSingleColumn DbCol.int32

int32_optional :: (SQL_IDENTIFIER dbTable)
               => dbTable
               -> AttributeTypeDatabaseInfo_same_optional dbTable Int32
int32_optional = atDbIo_forSingleColumn DbCol.int32_optional


-------------------------------------------------------------------------------
-- - Int64 -
-------------------------------------------------------------------------------


int64 :: (SQL_IDENTIFIER dbTable)
      => dbTable
      -> AttributeTypeDatabaseInfo_same dbTable Int64
int64 = atDbIo_forSingleColumn DbCol.int64

int64_optional :: (SQL_IDENTIFIER dbTable)
               => dbTable
               -> AttributeTypeDatabaseInfo_same_optional dbTable Int64
int64_optional = atDbIo_forSingleColumn DbCol.int64_optional


-------------------------------------------------------------------------------
-- - Word32 -
-------------------------------------------------------------------------------


word32 :: (SQL_IDENTIFIER dbTable)
       => dbTable
       -> AttributeTypeDatabaseInfo_same dbTable Word32
word32 = atDbIo_forSingleColumn DbCol.word32

word32_optional :: (SQL_IDENTIFIER dbTable)
                => dbTable
                -> AttributeTypeDatabaseInfo_same_optional dbTable Word32
word32_optional = atDbIo_forSingleColumn DbCol.word32_optional

word32_optionalOnCreate :: (SQL_IDENTIFIER dbTable)
                        => dbTable
                        -> AttributeTypeDatabaseInfo dbTable Word32 (Maybe Word32)
word32_optionalOnCreate = atDbIo_forSingleColumn_optionalOnCreate
                          DbCol.word32
                          DbCol.word32_optional


-------------------------------------------------------------------------------
-- - Word64 -
-------------------------------------------------------------------------------


word64 :: (SQL_IDENTIFIER dbTable)
       => dbTable
       -> AttributeTypeDatabaseInfo_same dbTable Word64
word64 = atDbIo_forSingleColumn DbCol.word64

word64_optional :: (SQL_IDENTIFIER dbTable)
                => dbTable
                -> AttributeTypeDatabaseInfo_same_optional dbTable Word64
word64_optional = atDbIo_forSingleColumn DbCol.word64_optional

word64_optionalOnCreate :: (SQL_IDENTIFIER dbTable)
                        => dbTable
                        -> AttributeTypeDatabaseInfo dbTable Word64 (Maybe Word64)
word64_optionalOnCreate = atDbIo_forSingleColumn_optionalOnCreate
                          DbCol.word64
                          DbCol.word64_optional


-------------------------------------------------------------------------------
-- - Double -
-------------------------------------------------------------------------------


double :: (SQL_IDENTIFIER dbTable)
       => dbTable
       -> AttributeTypeDatabaseInfo_same dbTable Double
double = atDbIo_forSingleColumn DbCol.double

double_optional :: (SQL_IDENTIFIER dbTable)
                => dbTable
                -> AttributeTypeDatabaseInfo_same_optional dbTable Double
double_optional = atDbIo_forSingleColumn DbCol.double_optional


-------------------------------------------------------------------------------
-- - String -
-------------------------------------------------------------------------------


-- | A mandatory String column, with "default" `DatabaseIo`
string :: (SQL_IDENTIFIER dbTable)
       => Int -- ^ Max string length
       -> dbTable -- ^ Column (of the table)
       -> AttributeTypeDatabaseInfo_same dbTable String
string maxLen = atDbIo_forSingleColumn (DbCol.string maxLen)

-- | An optional String column, with "default" `DatabaseIo`
string_optional :: (SQL_IDENTIFIER dbTable)
                => Int -- ^ Max string length
                -> dbTable -- ^ Column (of the table)
                -> AttributeTypeDatabaseInfo_same_optional dbTable String
string_optional maxLen = atDbIo_forSingleColumn (DbCol.string_optional maxLen)


-- | A mandatory String column, for a given `DatabaseIo`
string_forDbIo :: (SQL_IDENTIFIER dbTable)
               => DatabaseIo String
               -> Int -- ^ Max string length
               -> dbTable -- ^ Column (of the table)
               -> AttributeTypeDatabaseInfo_same dbTable String
string_forDbIo dbIo maxLen = atDbIo_forSingleColumn (DbCol.string_forDbIo dbIo maxLen)

-- | An optional String column, for a given `DatabaseIo`
string_optional_forDbIo :: (SQL_IDENTIFIER dbTable)
                        => DatabaseIo (Maybe String)
                        -> Int -- ^ Max string length
                        -> dbTable -- ^ Column (of the table)
                        -> AttributeTypeDatabaseInfo_same_optional dbTable String
string_optional_forDbIo dbIo maxLen = atDbIo_forSingleColumn (DbCol.string_optional_forDbIo dbIo maxLen)


-------------------------------------------------------------------------------
-- - LongString -
-------------------------------------------------------------------------------


longString :: (SQL_IDENTIFIER dbTable)
           => DatabaseIo String
           -> dbTable
           -> AttributeTypeDatabaseInfo_same dbTable String
longString dbIo = atDbIo_forSingleColumn (DbCol.longString dbIo)

longString_optional :: (SQL_IDENTIFIER dbTable)
                    => DatabaseIo String
                    -> dbTable
                    -> AttributeTypeDatabaseInfo_same_optional dbTable String
longString_optional dbIo = atDbIo_forSingleColumn (DbCol.longString_optional dbIo)


longString_forDefaultDbIo :: (SQL_IDENTIFIER dbTable)
                          => dbTable
                          -> AttributeTypeDatabaseInfo_same dbTable String
longString_forDefaultDbIo = atDbIo_forSingleColumn
                            DbCol.longString_forDefaultDbIo

longString_optional_forDefaultDbIo :: (SQL_IDENTIFIER dbTable)
                                   => dbTable
                                   -> AttributeTypeDatabaseInfo_same_optional dbTable String
longString_optional_forDefaultDbIo = atDbIo_forSingleColumn
                                     DbCol.longString_optional_forDefaultDbIo


-------------------------------------------------------------------------------
-- - Bool -
-------------------------------------------------------------------------------


bool :: (SQL_IDENTIFIER dbTable)
     => dbTable
     -> AttributeTypeDatabaseInfo_same dbTable Bool
bool = atDbIo_forSingleColumn DbCol.bool

bool_optional :: (SQL_IDENTIFIER dbTable)
              => dbTable
              -> AttributeTypeDatabaseInfo_same_optional dbTable Bool
bool_optional = atDbIo_forSingleColumn DbCol.bool_optional


-------------------------------------------------------------------------------
-- - Day -
-------------------------------------------------------------------------------


day :: (SQL_IDENTIFIER dbTable)
    => dbTable
    -> AttributeTypeDatabaseInfo_same dbTable Day
day = atDbIo_forSingleColumn DbCol.day

day_optional :: (SQL_IDENTIFIER dbTable)
             => dbTable
             -> AttributeTypeDatabaseInfo_same_optional dbTable Day
day_optional = atDbIo_forSingleColumn DbCol.day_optional


-------------------------------------------------------------------------------
-- - utilities -
-------------------------------------------------------------------------------


atDbIo_forSingleColumn :: (SQL_IDENTIFIER dbTable)
                       => DbCol.DatabaseColumnType a
                       -> dbTable
                       -> AttributeTypeDatabaseInfo dbTable a a
atDbIo_forSingleColumn (DbCol.DatabaseColumnType dbIo colDesc) column =
  AttributeTypeDatabaseInfo
  {
    atdbioInfoForExisting   =
       AttributeTypeDatabaseInfoForExisting
       {
         atdbiofeIo        = dbIo
       , atdbiofeStructure = NonEmpty.singleton col
       }
  , atdbioCreateOutputer = DbCol.mkOutputerWithConnection $
                           dbOutputer dbIo
  }
  where
    col = databaseColumn_withDdl colDesc column

atDbIo_forSingleColumn_optionalOnCreate :: (SQL_IDENTIFIER dbTable)
                                        => DbCol.DatabaseColumnType a
                                        -> DbCol.DatabaseColumnType (Maybe a)
                                        -> dbTable
                                        -> AttributeTypeDatabaseInfo dbTable a (Maybe a)
atDbIo_forSingleColumn_optionalOnCreate (DbCol.DatabaseColumnType dbIo_m colDesc_m)
  (DbCol.DatabaseColumnType dbIo_o colDesc_o) column =
  AttributeTypeDatabaseInfo
  {
    atdbioInfoForExisting   =
       AttributeTypeDatabaseInfoForExisting
       {
         atdbiofeIo        = dbIo_m
       , atdbiofeStructure = NonEmpty.singleton col
       }
  , atdbioCreateOutputer = DbCol.mkOutputerWithConnection $
                           dbOutputer dbIo_o
  }
  where
    col = databaseColumn_withDdl colDesc_m column

databaseColumn_withDdl :: (SQL_IDENTIFIER dbTable)
                       => SqlColDesc
                       -> dbTable
                       -> DdlColumnInfo dbTable
databaseColumn_withDdl = mkSimpleDdlColumnInfo

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
-- | Information about database structures for generating
-- SQL Data Definition Language (DDL) for an application's
-- Object Model.
-------------------------------------------------------------------------------
module Wilde.Database.SqlDdlInfo
       (
         -- DDL_INFORMATION(..),
         
         DdlColumnInfo(..),
         setColumnIdent,
         isForeignKey,
         mkSimpleDdlColumnInfo,
         addExtraWhenNotForeignKey,

         ForeignKeyTarget(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Database.HDBC.ColTypes

import Wilde.Database.Sql

import Wilde.Database.BackEndDdl


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Information about a database column for generating DDL for
-- CREATE TABLE
data DdlColumnInfo dbTable =
  SQL_IDENTIFIER dbTable => DdlColumnInfo
  {
    columnIdent            :: dbTable
  , hdbcColDesc            :: SqlColDesc
    -- | Information to include only when the column is not
    -- a foreign key.
  , extraWhenNotForeignKey :: [String]
    -- | Place for e.g. information that is specific for a database system.
  , columnExtra            :: [String]
  , foreignKey             :: Maybe ForeignKeyTarget
  }

-- | Record updater for setting the 'columnIdent' field.
--
-- (This cannot be update using normal record update syntax due to
-- the fact that 'columnIdent' is existentially quantified.)
setColumnIdent :: SQL_IDENTIFIER dbTable'
               => dbTable'
               -> DdlColumnInfo dbTable
               -> DdlColumnInfo dbTable'
setColumnIdent x (DdlColumnInfo
                  {
                    hdbcColDesc            = theHdbcColDesc
                  , extraWhenNotForeignKey = theExtraWhenNotForeignKey
                  , columnExtra            = theColumnExtra
                  , foreignKey             = theForeignKey
                  }) = 
  DdlColumnInfo
  {
    columnIdent            = x
  , hdbcColDesc            = theHdbcColDesc
  , extraWhenNotForeignKey = theExtraWhenNotForeignKey
  , columnExtra            = theColumnExtra
  , foreignKey             = theForeignKey
  }

-- | Constructs the simplest form of a 'DdlColumnInfo'.
--
-- Only the mandatory information.
-- Not a foreign key and no \"extra\" information.
mkSimpleDdlColumnInfo :: SQL_IDENTIFIER dbTable
                      => SqlColDesc -> dbTable -> DdlColumnInfo dbTable
mkSimpleDdlColumnInfo colDesc colName =
  DdlColumnInfo
  {
    columnIdent            = colName
  , hdbcColDesc            = colDesc
  , extraWhenNotForeignKey = []
  , columnExtra            = []
  , foreignKey             = Nothing
  }

-- | Tells if a 'DdlColumnInfo' represent a Foreign Key.
isForeignKey :: DdlColumnInfo dbTable -> Bool
isForeignKey = maybe False (const True) . foreignKey

-- | Adds elements to the list of 'extraWhenNotForeignKey':s.
addExtraWhenNotForeignKey :: [String] -> DdlColumnInfo dbTable -> DdlColumnInfo dbTable
addExtraWhenNotForeignKey extra dci =
  dci { extraWhenNotForeignKey = extra ++ curExtra }
  where
    curExtra = extraWhenNotForeignKey dci

-- class DDL_INFORMATION dbInfo where
--   ddlColumnIdentifer :: dbInfo -> SqlIdentifier
--   ddlHdbcColDesc     :: dbInfo -> SqlColDesc
--   ddlColumnExtra     :: dbInfo -> Maybe String

-- instance SQL_IDENTIFIER dbTable => SQL_IDENTIFIER (DdlColumnInfo dbTable) where
--   sqlIdentifier = sqlIdentifier . columnIdent

-- instance SQL_IDENTIFIER dbTable => DDL_INFORMATION (DdlColumnInfo dbTable) where
--   ddlColumnIdentifer = sqlIdentifier . columnIdent
--   ddlHdbcColDesc     = hdbcColDesc
--   ddlColumnExtra     = columnExtra

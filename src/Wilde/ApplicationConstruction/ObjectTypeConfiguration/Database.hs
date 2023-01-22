{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

-- | 'ObjectType' configuration that supports database IO.
module Wilde.ApplicationConstruction.ObjectTypeConfiguration.Database
       (
         Configuration(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Media.Database
import Wilde.ObjectModel.ObjectModel
import Wilde.ObjectModel.Database


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data Configuration dbTable otNative idAtExisting idAtCreate =
  Configuration
  {
    databaseTable               :: DatabaseTable
  , getIdOfInsertedIntoDatabase :: GetIdOfInsertedIntoDatabase idAtExisting idAtCreate
  }

instance DATABASE_TABLE Configuration where
  otDatabaseTable = databaseTable . otConfiguration

instance OBJECT_TYPE_INSERT Configuration where
  otDatabaseGetIdOfInserted = getIdOfInsertedIntoDatabase .
                              otConfiguration

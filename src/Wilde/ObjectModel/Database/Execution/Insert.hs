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
-- | Insertion of objects and immediate reading of them as \"plain\"
-- objects.
-------------------------------------------------------------------------------
module Wilde.ObjectModel.Database.Execution.Insert
       (
         insertOne,
         insertOneGetId,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Media.Database (DatabaseOutput)
import qualified Wilde.Media.Database.Exec as SqlExec

import Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.ObjectModelUtils as OmUtils

import qualified Wilde.Media.Database.Monad as DbConn

import qualified Wilde.ObjectModel.Database.Sql.SansPresentationInfo as SqlPlain
import           Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.Database.Output as Output


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inserts one 'ObjectForCreate' and returns the 'DatabaseOutput' for
-- the ID 'AttributeType'.
-------------------------------------------------------------------------------
insertOne :: (Database.DATABASE_TABLE otConf
             ,Database.COLUMN_NAMES atConf
             ,Output.OUTPUT_FOR_CREATE atConf)
          => ObjectForCreate otConf atConf dbTable otNative idAtExisting idAtCreate
          -> DbConn.Monad DatabaseOutput
insertOne oc@(ObjectForCreate {}) =
  do
    (sqlValuesIdAt,sqlValuesNonIdAts) <- databaseOutputForInsert oc
    let sqlValuesAllAts = sqlValuesIdAt ++ sqlValuesNonIdAts
    let sql = SqlPlain.insertOne (ofcType oc)
    SqlExec.insert sql sqlValuesAllAts
    return sqlValuesIdAt

-------------------------------------------------------------------------------
-- | Output for (ID-AT,non-ID-ATs).
-------------------------------------------------------------------------------
databaseOutputForInsert :: Output.OUTPUT_FOR_CREATE atConf
                        => ObjectForCreate otConf atConf dbTable otNative idAtExisting idAtCreate
                        -> DbConn.Monad (DatabaseOutput,DatabaseOutput)
databaseOutputForInsert oc =
  do
    forIdAt     <- getForIdAt
    forNonIdAts <- sequence getForNonIdAts
    return (forIdAt,concat forNonIdAts)
  where
    ot             = ofcType oc
    getForIdAt     = Output.aOutputForCreate $ ofcIdAttribute oc
    getForNonIdAts = map
                     (OmUtils.anyValueApply Output.aOutputForCreate)
                     (ofcNonIdAttributes oc)

-------------------------------------------------------------------------------
-- | Inserts one object and reads the ID from the database.
-------------------------------------------------------------------------------
insertOneGetId :: (Database.OBJECT_TYPE_INSERT otConf
                  ,Database.COLUMN_NAMES atConf
                  ,Output.OUTPUT_FOR_CREATE atConf)
               => ObjectForCreate otConf atConf dbTable otNative idAtExisting idAtCreate
               -> DbConn.Monad idAtExisting
insertOneGetId oc =
  do
    insertDbOutputForIdAt <- insertOne oc
    getIdOfInsertedObject (attrfcValue (ofcIdAttribute oc)) insertDbOutputForIdAt
  where
    getIdOfInsertedObject = otDatabaseGetIdOfInserted $ ofcType oc

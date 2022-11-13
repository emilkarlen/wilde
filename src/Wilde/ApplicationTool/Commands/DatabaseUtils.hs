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

module Wilde.ApplicationTool.Commands.DatabaseUtils
       (
         getObjects,
         getObjectsWithConn,
         runDatabaseMonad_inEnv,
         runDatabaseMonad_inCar,
       )
       where



-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Media.Database.Monad as DbConn
import qualified Wilde.Media.Database.Exec as DbExec

import qualified Wilde.ApplicationTool.DbExecution as SqlExec

import qualified Wilde.Media.ElementSet as ES (empty,ElementSet)

import qualified Wilde.ObjectModel.Database.Sql.SansPresentationInfo as SqlPlain
import qualified Wilde.ObjectModel.Database.Execution.SelectWithPresentationInfo as InputWithPresentation
import qualified Wilde.ObjectModel.Database.InputExistingSansPresentationInfo as InputExisting
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation

import Wilde.ObjectModel.ObjectModelUtils

import Wilde.Media.Database (runTranslation)

import Wilde.ApplicationTool.Command

import Wilde.ApplicationTool.ErrorHandling


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


getObjects :: (Database.DATABASE_TABLE otConf
              ,Database.COLUMN_NAMES atConf
              ,Database.INPUT_FOR_EXISTING atConf
              )
           => CommandEnv om
           -> ObjectType otConf atConf dbTable otN idAE idAC
           -> IO [Object otConf atConf dbTable otN idAE idAC]
getObjects env objectType@(ObjectType {}) =
  do
    let sql = SqlPlain.selectAll objectType []
    car    <- getCar env
    rows   <- runDatabaseMonad_inEnv env $ DbExec.select_strict sql []
    let osRes = runTranslation customEnvironment $ 
                mapM
                (InputExisting.inputObject objectType) 
                rows
    case osRes of
      Left msg      -> msgFail $ "Error: " ++ show msg
      Right objects -> return objects

getObjectsWithConn :: (Database.DATABASE_TABLE otConf
                      ,Database.INPUT_FOR_EXISTING atConf
                      ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
                      )
                   => SqlExec.ConnectionAndRenderer
                   -> ObjectType otConf atConf dbTable otN idAE idAC
                   -> IO [Object otConf atConf dbTable otN idAE idAC]
getObjectsWithConn car objectType = 
  runDatabaseMonad_inCar car dbMonad
  where
    dbMonad = InputWithPresentation.inputAll objectType []

customEnvironment :: ES.ElementSet
customEnvironment = ES.empty

runDatabaseMonad_inEnv :: CommandEnv om -> DbConn.Monad a -> IO a
runDatabaseMonad_inEnv (CommandEnv { connectionProvider = getConn, dmlRenderer = renderer}) action = do
  conn <- getConn
  let dbConnEnv = DbConn.newEnv customEnvironment renderer conn
  errOrResult <- DbConn.run dbConnEnv action
  either doFail pure errOrResult
  where
    doFail msg = msgFail $ "Database Error: " ++ show msg

runDatabaseMonad_inCar :: SqlExec.ConnectionAndRenderer -> DbConn.Monad a -> IO a
runDatabaseMonad_inCar (SqlExec.ConnectionAndRenderer { SqlExec.carConnection = conn, SqlExec.carRenderer = renderer}) action = do
  let dbConnEnv = DbConn.newEnv customEnvironment renderer conn
  errOrResult <- DbConn.run dbConnEnv action
  either doFail pure errOrResult
  where
    doFail msg = msgFail $ "Database Error: " ++ show msg

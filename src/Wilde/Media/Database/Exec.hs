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

-- | SQL execution in the DbConnMonad.
module Wilde.Media.Database.Exec
       (
         DbConnM.Monad,

         DbConnM.ToMonad(..),
         ToDatabaseError(..),

         -- * SQL execution
         select_strict,
         select_lazy,
         insert,
         update,
         delete,

         execSql_numRows,
         execSql_numRowsMb,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Control.Monad.IO.Class

import Database.HDBC (SqlValue)
import qualified Database.HDBC as HDBC

import qualified Wilde.Utils.Logging.Monad as Logging

import qualified Wilde.Media.Database.Monad as DbConnM
-- prepare :: IConnection conn => conn -> String -> IO Statement
-- execute :: Statement -> [SqlValue] -> IO Integer

import Wilde.Media.Database.Error

import Wilde.Database.Sql (SQL_IDENTIFIER(..), SqlDmlStatement(..),
                            SqlSelect, SqlInsert, SqlUpdate, SqlDelete)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Executes a non-SELECT statement,
-- returning the numer of rows operated upon.
execSql_numRows :: SQL_IDENTIFIER col
                => SqlDmlStatement col
                -> [SqlValue] -- ^ positional parameters
                -> DbConnM.Monad Integer
execSql_numRows sql params = Logging.loggBeginEnd Logging.LIBRARY "execSql_numRows" $ do
  Logging.logg Logging.LIBRARY "execSql_numRows: prepareSql..."
  stmt <- DbConnM.prepareSql sql
  Logging.logg Logging.LIBRARY "execSql_numRows: HDBC.execute..."
  liftIO $ HDBC.execute stmt params

-- | Executes a non-SELECT statement,
-- returning the numer of rows operated upon.
--
-- Nothing is returned if the number of rows
-- operated upon is not supported by the DB backend.
execSql_numRowsMb :: SQL_IDENTIFIER col
                  => SqlDmlStatement col
                  -> [SqlValue] -- ^ positional parameters
                  -> DbConnM.Monad (Maybe Integer)
execSql_numRowsMb sql params = Logging.loggBeginEnd Logging.LIBRARY "execSql_numRowsMb" $ do
  numRows <- execSql_numRows sql params
  pure $ fixNumRows numRows
  where
    fixNumRows :: Integer -> Maybe Integer
    fixNumRows n =
      if n < 0
      then Nothing
      else Just n

select_lazy :: SQL_IDENTIFIER col
            => SqlSelect col
            -> [SqlValue] -- ^ positional parameters
            -> DbConnM.Monad [[SqlValue]]
select_lazy sql params = Logging.loggBeginEnd Logging.LIBRARY "select_lazy" $ do
  stmt <- DbConnM.prepareSql (SqlDmlSelect sql)
  liftIO $ do
    HDBC.execute stmt params
    HDBC.fetchAllRows stmt

select_strict :: SQL_IDENTIFIER col
              => SqlSelect col
              -> [SqlValue] -- ^ positional parameters
              -> DbConnM.Monad [[SqlValue]]
select_strict sql params = Logging.loggBeginEnd Logging.LIBRARY "select_strict" $ do
  stmt <- DbConnM.prepareSql (SqlDmlSelect sql)
  liftIO $ do
    HDBC.execute stmt params
    HDBC.fetchAllRows' stmt

insert :: SQL_IDENTIFIER col
       => SqlInsert col
       -> [SqlValue] -- ^ positional parameters
       -> DbConnM.Monad Integer
insert sql params = Logging.loggBeginEnd Logging.LIBRARY "insert" $ execSql_numRows (SqlDmlInsert sql) params

update :: SQL_IDENTIFIER col
       => SqlUpdate col
       -> [SqlValue] -- ^ positional parameters
       -> DbConnM.Monad Integer
update sql params = Logging.loggBeginEnd Logging.LIBRARY "update" $ execSql_numRows (SqlDmlUpdate sql) params

delete :: SQL_IDENTIFIER col
       => SqlDelete col
       -> [SqlValue] -- ^ positional parameters
       -> DbConnM.Monad Integer
delete sql params = Logging.loggBeginEnd Logging.LIBRARY "delete" $ execSql_numRows (SqlDmlDelete sql) params

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
module Wilde.Media.Database.ExecStr
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

import qualified Wilde.Media.Database.Monad as DbConnM
-- prepare :: IConnection conn => conn -> String -> IO Statement
-- execute :: Statement -> [SqlValue] -> IO Integer

import Wilde.Media.Database.Error


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Executes a non-SELECT statement,
-- returning the numer of rows operated upon.
execSql_numRows :: String -- ^ SQL DML statement
                -> [SqlValue] -- ^ positional parameters
                -> DbConnM.Monad Integer
execSql_numRows sql params = DbConnM.loggBeginEnd "execSql_numRows/str" $ do
  DbConnM.logg "execSql_numRows: prepareSql..."
  stmt <- DbConnM.prepareSql_str sql
  DbConnM.logg "execSql_numRows: HDBC.execute..."
  liftIO $ HDBC.execute stmt params

-- | Executes a non-SELECT statement,
-- returning the numer of rows operated upon.
--
-- Nothing is returned if the number of rows
-- operated upon is not supported by the DB backend.
execSql_numRowsMb :: String -- ^ SQL DML statement
                  -> [SqlValue] -- ^ positional parameters
                  -> DbConnM.Monad (Maybe Integer)
execSql_numRowsMb sql params = DbConnM.loggBeginEnd "execSql_numRowsMb/str" $ do
  numRows <- execSql_numRows sql params
  pure $ fixNumRows numRows
  where
    fixNumRows :: Integer -> Maybe Integer
    fixNumRows n =
      if n < 0
      then Nothing
      else Just n

select_lazy :: String     -- ^ SQL SELECT statement
            -> [SqlValue] -- ^ positional parameters
            -> DbConnM.Monad [[SqlValue]]
select_lazy sql params = DbConnM.loggBeginEnd "select_lazy/str" $ do
  stmt <- DbConnM.prepareSql_str sql
  liftIO $ do
    HDBC.execute stmt params
    HDBC.fetchAllRows stmt

select_strict :: String     -- ^ SQL SELECT statement
              -> [SqlValue] -- ^ positional parameters
              -> DbConnM.Monad [[SqlValue]]
select_strict sql params = DbConnM.loggBeginEnd "select_strict/str" $ do
  stmt <- DbConnM.prepareSql_str sql
  liftIO $ do
    HDBC.execute stmt params
    HDBC.fetchAllRows' stmt

insert :: String     -- ^ SQL INSERT statement
       -> [SqlValue] -- ^ positional parameters
       -> DbConnM.Monad Integer
insert sql params = DbConnM.loggBeginEnd "insert/str" $ execSql_numRows sql params

update :: String     -- ^ SQL UPDATE statement
       -> [SqlValue] -- ^ positional parameters
       -> DbConnM.Monad Integer
update sql params = DbConnM.loggBeginEnd "update/str" $ execSql_numRows sql params

delete :: String     -- ^ SQL DELETE statement
       -> [SqlValue] -- ^ positional parameters
       -> DbConnM.Monad Integer
delete sql params = DbConnM.loggBeginEnd "delete/str" $ execSql_numRows sql params

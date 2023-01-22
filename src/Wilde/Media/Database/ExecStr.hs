-- | SQL execution in the DbConnMonad.
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Wilde.Utils.Logging.Monad as Logging
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
execSql_numRows sql params = Logging.loggBeginEnd Logging.LIBRARY "execSql_numRows/str" $ do
  Logging.logg Logging.LIBRARY "execSql_numRows: prepareSql..."
  stmt <- DbConnM.prepareSql_str sql
  Logging.logg Logging.LIBRARY "execSql_numRows: HDBC.execute..."
  liftIO $ HDBC.execute stmt params

-- | Executes a non-SELECT statement,
-- returning the numer of rows operated upon.
--
-- Nothing is returned if the number of rows
-- operated upon is not supported by the DB backend.
execSql_numRowsMb :: String -- ^ SQL DML statement
                  -> [SqlValue] -- ^ positional parameters
                  -> DbConnM.Monad (Maybe Integer)
execSql_numRowsMb sql params = Logging.loggBeginEnd Logging.LIBRARY "execSql_numRowsMb/str" $ do
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
select_lazy sql params = Logging.loggBeginEnd Logging.LIBRARY "select_lazy/str" $ do
  stmt <- DbConnM.prepareSql_str sql
  liftIO $ do
    HDBC.execute stmt params
    HDBC.fetchAllRows stmt

select_strict :: String     -- ^ SQL SELECT statement
              -> [SqlValue] -- ^ positional parameters
              -> DbConnM.Monad [[SqlValue]]
select_strict sql params = Logging.loggBeginEnd Logging.LIBRARY "select_strict/str" $ do
  stmt <- DbConnM.prepareSql_str sql
  liftIO $ do
    HDBC.execute stmt params
    HDBC.fetchAllRows' stmt

insert :: String     -- ^ SQL INSERT statement
       -> [SqlValue] -- ^ positional parameters
       -> DbConnM.Monad Integer
insert sql params = Logging.loggBeginEnd Logging.LIBRARY "insert/str" $ execSql_numRows sql params

update :: String     -- ^ SQL UPDATE statement
       -> [SqlValue] -- ^ positional parameters
       -> DbConnM.Monad Integer
update sql params = Logging.loggBeginEnd Logging.LIBRARY "update/str" $ execSql_numRows sql params

delete :: String     -- ^ SQL DELETE statement
       -> [SqlValue] -- ^ positional parameters
       -> DbConnM.Monad Integer
delete sql params = Logging.loggBeginEnd Logging.LIBRARY "delete/str" $ execSql_numRows sql params

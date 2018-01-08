-- | Connection to an ODBC source.
module Common.App.DatabaseConnect
       (
         connectToDsn,
         dmlRenderer,
         dmlRendererAny,
         dmlExecutorConfiguration,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Database.HDBC
import Database.HDBC.MySQL
import Database.HDBC.ODBC

import Wilde.Database.Sql

import qualified Wilde.Driver.Database.MySQL.DmlExcutor as DmlExecutor


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


dmlExecutorConfiguration :: String -> DmlExecutor.Configuration
dmlExecutorConfiguration dataSourceName =
  DmlExecutor.Configuration
  {
    DmlExecutor.connectionProvider = connectToDsn dataSourceName
  , DmlExecutor.dmlRenderer        = DmlExecutor.mysqlDmlRenderer
  }

dmlRenderer :: DmlExecutor.DmlRenderer
dmlRenderer = DmlExecutor.mysqlDmlRenderer

dmlRendererAny :: SQL_IDENTIFIER col => SqlDmlStatement col -> String
dmlRendererAny = dmlRenderer . fmap sqlIdentifier

connectToDsn :: String -> IO ConnWrapper
connectToDsn = connectToDsnMysql

connectToDsnOdbc :: String -> IO ConnWrapper
connectToDsnOdbc dataSourceName =
  do
    conn <- connectODBC connectionString
    return $ ConnWrapper conn
  where
    connectionString = "DSN=" ++ dataSourceName

connectToDsnMysql :: String -> IO ConnWrapper
connectToDsnMysql dataSourceName =
  do
    conn <- connectMySQL $ mysqlConnectionInfo dataSourceName
    return $ ConnWrapper conn

mysqlConnectionInfo databaseName = defaultMySQLConnectInfo {
  mysqlUnixSocket = "/var/run/mysqld/mysqld.sock",
  mysqlHost       = "localhost",
  mysqlUser       = "dml",
  mysqlDatabase   = databaseName
  }

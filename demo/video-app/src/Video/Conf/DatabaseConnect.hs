module Video.Conf.DatabaseConnect
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


import           Database.HDBC
import           Database.HDBC.MariaDB
-- import Database.HDBC.ODBC

import           Wilde.Database.Sql

import qualified Wilde.Media.Database.Configuration as DbConf
import qualified Wilde.Driver.Database.MySQL.DmlExcutor as DmlExecutor


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


dmlExecutorConfiguration :: String -> DbConf.Configuration
dmlExecutorConfiguration dataSourceName =
  DbConf.Configuration
  {
    DbConf.connectionProvider = connectToDsn dataSourceName
  , DbConf.dmlRenderer        = DmlExecutor.mysqlDmlRenderer
  }

dmlRenderer :: DmlExecutor.DmlRenderer
dmlRenderer = DmlExecutor.mysqlDmlRenderer

dmlRendererAny :: SQL_IDENTIFIER col => SqlDmlStatement col -> String
dmlRendererAny = dmlRenderer . fmap sqlIdentifier

connectToDsn :: String -> IO ConnWrapper
connectToDsn = connectToDsn_mariadb
-- connectToDsn = connectToDsn_odbc

-- connectToDsn_odbc :: String -> IO ConnWrapper
-- connectToDsn_odbc dataSourceName =
--   do
--     conn <- connectODBC connectionString
--     pure $ ConnWrapper conn
--   where
--     connectionString = "DSN=" ++ dataSourceName

connectToDsn_mariadb :: String -> IO ConnWrapper
connectToDsn_mariadb dataSourceName =
  do
    conn <- connectMariaDB $ mysqlConnectionInfo dataSourceName
    pure $ ConnWrapper conn

mysqlConnectionInfo databaseName = defaultMariaDBConnectInfo {
  mysqlUnixSocket = "/var/run/mysqld/mysqld.sock",
  mysqlHost       = "localhost",
  mysqlUser       = "video_user_dml",
  mysqlDatabase   = databaseName
  }

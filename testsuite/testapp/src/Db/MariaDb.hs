-- | Connection to the cashflow database schema.
module Db.MariaDb
       (
         newConf,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Database.HDBC
import qualified Database.HDBC.MariaDB as MariaDB

import qualified Wilde.Media.Database.Configuration as DbConf
import qualified Wilde.Driver.Database.MySQL.DmlExcutor as DmlExcutor


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


newConf :: String -- ^ DB name
        -> String -- ^ DB user name
        -> DbConf.Configuration
newConf db user =
  DbConf.Configuration
  {
    DbConf.connectionProvider = getConnection
  , DbConf.dmlRenderer        = DmlExcutor.mysqlDmlRenderer
  }
  where
    getConnection :: IO ConnWrapper
    getConnection =
      do
        conn <- MariaDB.connectMariaDB connectionInfo
        pure $ ConnWrapper conn

    connectionInfo = MariaDB.defaultMariaDBConnectInfo {
      MariaDB.mysqlUnixSocket = "/var/run/mysqld/mysqld.sock",
      MariaDB.mysqlHost       = "localhost",
      MariaDB.mysqlUser       = user,
      MariaDB.mysqlDatabase   = db
    }

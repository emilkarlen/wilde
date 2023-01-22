module Db.Odbc
       (
         connect,
         theDbConfiguration,
       )

       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Database.HDBC
import qualified Database.HDBC.ODBC as ODBC

import qualified Wilde.Driver.Database.MySQL.DmlExcutor as DmlExecutor


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------

newConf :: String -- ^ DB name
        -> DmlExecutor.Configuration
newConf db =
  DmlExecutor.Configuration
  {
    DmlExecutor.connectionProvider = connect
  , DmlExecutor.dmlRenderer        = DmlExecutor.mysqlDmlRenderer
  }
  where
    odbcConnectionString = "DSN=" ++ db

    connect :: IO ConnWrapper
    connect =
      do
        conn <- ODBC.connectODBC odbcConnectionString
        pure $ ConnWrapper conn

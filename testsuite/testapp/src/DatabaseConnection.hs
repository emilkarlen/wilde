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

module DatabaseConnection
       (
         connect,
         theDbConfiguration,
       )
       
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Database.HDBC
import Database.HDBC.ODBC

import qualified Wilde.Driver.Database.MySQL.DmlExcutor as DmlExecutor


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


odbcDataSourceName = "wilde_test"
odbcConnectionString = "DSN=" ++ odbcDataSourceName

connect :: IO ConnWrapper
connect =
  do
    conn <- connectODBC odbcConnectionString
    return $ ConnWrapper conn

theDbConfiguration :: DmlExecutor.Configuration
theDbConfiguration =
  DmlExecutor.Configuration
  {
    DmlExecutor.connectionProvider = connect
  , DmlExecutor.dmlRenderer        = DmlExecutor.mysqlDmlRenderer
  }

-- socket       = "/var/run/mysqld/mysqld.sock"
-- hostName     = "localhost"
-- databaseName = "wilde_test"
-- user         = "developer"
-- password     = ""

-- testConnect :: IO ConnWrapper
-- testConnect =
--   do
--     conn <- connectMySql defaultMySqlConnectInfo {
--       mysqlUnixSocket = socket,
--       mysqlHost       = hostName,
--       mysqlUser       = user,
--       mysqlDatabase   = databaseName
--       }
--     return $ ConnWrapper conn

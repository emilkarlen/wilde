module Main where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Database.HDBC.MariaDB as MariaDB

import qualified Wilde.Driver.Database.MySQL.Renderer as MySql
import qualified Wilde.Driver.Database.MySQL.DmlExcutor as DmlExecutor

import qualified Wilde.ApplicationTool.Main as ToolMain
import qualified Wilde.ApplicationTool.DefaultCommands as DefaultCommands
import qualified Wilde.ApplicationTool.ApplicationModel as ApplicationModel

import qualified Video.Conf.DatabaseConnect as DbConnect

import qualified Video.ObjectModel as ObjectModel
import qualified Video.Conf.AppConf as AppConf


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


main :: IO ()
main =
  MariaDB.withRTSSignalsBlocked $
  ToolMain.cliToolMain
  commandEnv
  DefaultCommands.commands

commandEnv = ToolMain.CommandEnv
  {
    ToolMain.connectionProvider = DbConnect.connectToDsn AppConf.dbName
  , ToolMain.ddlRenderer        = MySql.renderer
  , ToolMain.dmlRenderer        = DmlExecutor.mysqlDmlRenderer
  , ToolMain.objectModel        = ApplicationModel.ObjectModel
                                  ObjectModel.objectModelSetup []
  }

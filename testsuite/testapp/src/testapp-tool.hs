module Main where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Media.Database.Configuration as DbConf

import qualified ObjectModel
import qualified Db.Connection as DbConn

import qualified Wilde.Driver.Database.MySQL.Renderer as MySql

import qualified Wilde.ApplicationTool.Main as ToolMain
import qualified Wilde.ApplicationTool.DefaultCommands as DefaultCommands
import qualified Wilde.ApplicationTool.ApplicationModel as ApplicationModel


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


main = ToolMain.cliToolMain commandEnv DefaultCommands.commands
  where
    commandEnv = ToolMain.CommandEnv
                 {
                   ToolMain.connectionProvider = DbConf.connectionProvider dbConf
                 , ToolMain.ddlRenderer        = MySql.renderer
                 , ToolMain.dmlRenderer        = DbConf.dmlRenderer dbConf
                 , ToolMain.objectModel        = ApplicationModel.ObjectModel ObjectModel.objectModel []
                 }

    dbConf = DbConn.theDbConfiguration

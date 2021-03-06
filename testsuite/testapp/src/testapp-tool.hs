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

module Main where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Driver.Database.MySQL.DmlExcutor as DmlExecutor

import TestApplication
import DatabaseConnection

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
                   ToolMain.connectionProvider = connect
                 , ToolMain.ddlRenderer        = MySql.renderer
                 , ToolMain.dmlRenderer        = DmlExecutor.dmlRenderer theDbConfiguration
                 , ToolMain.objectModel        = ApplicationModel.ObjectModel objectModel []
                 }

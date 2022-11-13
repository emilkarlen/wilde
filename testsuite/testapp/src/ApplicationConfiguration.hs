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

module ApplicationConfiguration where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Application.ApplicationConfiguration as AppConf

import Db.Connection as DbConn

import qualified Wilde.Driver.UserInteraction.Translation.En as Tr
import qualified Wilde.Driver.UserInteraction.StandardServiceLinkRenderer as StandardServiceLinkRenderer

import qualified Wilde.Driver.Application.Cgi.ServiceLinkRenderers as CgiDriver

import qualified Services

-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------



cssFileName :: String
cssFileName = "wilde_test.css"

appConfig :: AppConf.ApplicationConfiguration
appConfig =  AppConf.ApplicationConfiguration         
             {
               AppConf.appServices                     = Services.services
             , AppConf.appCssFile                      = Just cssFileName
             , AppConf.translations                    = Tr.translations
             , AppConf.dbConfiguration                 = DbConn.theDbConfiguration
             , AppConf.standardServiceLinkRenderer     = StandardServiceLinkRenderer.renderer
             , AppConf.getStdObjectTypeServiceRenderer = CgiDriver.getStandardObjectTypeServiceLinkRenderer
             , AppConf.getStdObjectServiceRenderer     = CgiDriver.getStandardObjectServiceLinkRenderer
             , AppConf.getGenericServiceLinkRenderer   = CgiDriver.getGenericServiceLinkRenderer
             }

module ApplicationConfiguration where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified System.IO as IO
import qualified Data.Text.IO as TIO

import           Wilde.Render.Html.Types

import qualified Wilde.Utils.Logging.Class as Logging
import qualified Wilde.Utils.Logging.SimpleLogger as SimpleLogger

import qualified Wilde.Application.ApplicationConfiguration as AppConf

import qualified Wilde.Driver.UserInteraction.Translation.En as Tr
import qualified Wilde.Driver.UserInteraction.StandardServiceLinkRenderer as StandardServiceLinkRenderer

import qualified Wilde.Driver.Application.Cgi.ServiceLinkRenderers as CgiDriver

import Db.Connection as DbConn

import qualified Services


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


cssLocLib, cssLocApp :: URL
cssLocLib = "/style-lib/"
cssLocApp = "/style-app/"

cssFileNameLib, cssFileNameApp :: String
cssFileNameLib = "wilde.css"
cssFileNameApp = "application.css"

newLogger :: Logging.Level -> Logging.AnyLogger
newLogger level = SimpleLogger.newLogger (TIO.hPutStrLn IO.stderr) level mempty

newAppConfig :: Logging.AnyLogger -> AppConf.ApplicationConfiguration
newAppConfig logger =  AppConf.ApplicationConfiguration
  {
    AppConf.appServices                 = Services.services
  , AppConf.appCssFiles                 = [ cssLocLib <> cssFileNameLib
                                          , cssLocApp <> cssFileNameApp ]
  , AppConf.translations                = Tr.translations
  , AppConf.dbConfiguration             = DbConn.theDbConfiguration
  , AppConf.appLogger                   = logger
  , AppConf.serviceLinks                =
    AppConf.ServiceLinks
    {
      AppConf.standardServiceLinkRenderer = StandardServiceLinkRenderer.renderer
    , AppConf.mkStdObjectTypeServiceLink  = CgiDriver.getMkStandardObjectTypeServiceLink
    , AppConf.mkStdObjectServiceLink      = CgiDriver.getMkStandardObjectServiceLink
    , AppConf.mkGenericServiceLink        = CgiDriver.getMkGenericServiceLink
    }
  }

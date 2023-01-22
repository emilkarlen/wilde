module ApplicationConfiguration where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified System.IO as IO
import qualified Data.Text.IO as TIO

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


cssFileName :: String
cssFileName = "wilde_test.css"

newLogger :: Logging.Level -> Logging.AnyLogger
newLogger level = SimpleLogger.newLogger (TIO.hPutStrLn IO.stderr) level mempty

newAppConfig :: Logging.AnyLogger -> AppConf.ApplicationConfiguration
newAppConfig logger =  AppConf.ApplicationConfiguration
  {
    AppConf.appServices                 = Services.services
  , AppConf.appCssFile                  = Just cssFileName
  , AppConf.translations                = Tr.translations
  , AppConf.dbConfiguration             = DbConn.theDbConfiguration
  , AppConf.standardServiceLinkRenderer = StandardServiceLinkRenderer.renderer
  , AppConf.getMkStdObjectTypeService   = CgiDriver.getMkStandardObjectTypeServiceLink
  , AppConf.getMkStdObjectService       = CgiDriver.getMkStandardObjectServiceLink
  , AppConf.getMkGenericServiceLink     = CgiDriver.getMkGenericServiceLink
  , AppConf.appLogger                   = logger
  }

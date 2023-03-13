{-# LANGUAGE OverloadedStrings #-}

module Video.Conf.AppConf where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.Text hiding (map)

import qualified Wilde.Driver.UserInteraction.Translation.En as Tr
import qualified Wilde.Driver.Application.Cgi.ServiceLinkRenderers as CgiDriver

import           Wilde.ApplicationConstruction.StandardServices
import           Wilde.ApplicationConstruction.Database.DatabaseUtils

import           Wilde.Application.ApplicationConfiguration

import qualified Wilde.Utils.Logging.Class as Logging

import qualified Video.Ui.LinkRenderer as LinkRenderer
import qualified Video.Conf.DatabaseConnect as DatabaseConnect

import qualified Video.ObjectModel as ObjectModel


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


dbName :: String
dbName = "video"

services :: ApplicationServices
services = standardServices $ map anyOWithDdlInfo2AnyO ObjectModel.objectModelSetup

cssFilePath :: FilePath
cssFilePath = "css-dir"

cssFileName_lib, cssFileName_app :: String
cssFileName_lib = "wilde.css"
cssFileName_app = "video-custom.css"

cssPath_lib, cssPath_app :: FilePath
cssPath_lib = "/style/lib/"
cssPath_app = "/style/app/"

cssRequestPath_lib, cssRequestPath_app :: [Text]
cssRequestPath_lib = ["style", "lib"]
cssRequestPath_app = ["style", "app"]

newAppConf :: Logging.AnyLogger
           -> ApplicationConfiguration
newAppConf logger =
    ApplicationConfiguration
    {
      appServices     = services
    , appCssFiles     = [ cssPath_lib <> cssFileName_lib
                        , cssPath_app <> cssFileName_app
                        ]
    , translations    = Tr.translations
    , dbConfiguration = DatabaseConnect.dmlExecutorConfiguration dbName
    , appLogger       = logger
    , serviceLinks    =
      ServiceLinks
      {
        standardServiceLinkRenderer  = LinkRenderer.standardServiceLinkRenderer
      , mkStdObjectTypeServiceLink   = CgiDriver.getMkStandardObjectTypeServiceLink
      , mkStdObjectServiceLink       = CgiDriver.getMkStandardObjectServiceLink
      , mkGenericServiceLink         = CgiDriver.getMkGenericServiceLink
      }
    }

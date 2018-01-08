module Main where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Database.HDBC.MySQL

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.CGI as WaiCGI

import Wilde.ApplicationConstruction.StandardServices
import Wilde.ApplicationConstruction.Database.DatabaseUtils

import qualified Wilde.Driver.UserInteraction.Translation.Se as Tr
import           Wilde.Driver.Application.Wai.Application

import           Common.App.WaiSystemConfiguration

import qualified Common.App.Ui.LinkRenderer as LinkRenderer
import qualified Common.App.DatabaseConnect as DatabaseConnect

import qualified Video.ObjectModel as ObjectModel

-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


main :: IO ()
main = withRTSSignalsBlocked $ WaiCGI.run waiApp

waiApp :: Wai.Application
waiApp = csApplication systemConfiguration appConfig

services :: ApplicationServices
services = standardServices $ map anyOWithDdlInfo2AnyO ObjectModel.objectModelSetup

cssFileName :: String
cssFileName = "video.css"

cssFilePath :: Maybe String
cssFilePath = Just $ "/style/css/" ++ cssFileName

odbcDataSourceName = "video"

appConfig :: ApplicationConfiguration
appConfig =  ApplicationConfiguration         
             {
               appServices                 = services
             , appCssFile                  = cssFilePath
             , translations                = Tr.translations
             , dbConfiguration             = DatabaseConnect.dmlExecutorConfiguration
                                             odbcDataSourceName
             , standardServiceLinkRenderer = LinkRenderer.standardServiceLinkRenderer
             }

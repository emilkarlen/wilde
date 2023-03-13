module Main where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Database.HDBC.MariaDB as MariaDB

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.CGI as WaiCGI

import qualified Wilde.Driver.Application.Cgi.Wai as Wai
import qualified Wilde.Utils.Logging.NoLogging as NoLogging

import qualified Video.Conf.WaiSystemConfiguration as WaiSystemConfiguration

import qualified Video.Conf.AppConf as AppConf


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


main :: IO ()
main  = MariaDB.withRTSSignalsBlocked $ WaiCGI.run waiApp

waiApp :: Wai.Application
waiApp  = Wai.newApplication WaiSystemConfiguration.codingConfiguration appConfig

appConfig :: Wai.ApplicationConfiguration
appConfig  = AppConf.newAppConf NoLogging.theLogger

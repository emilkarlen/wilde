-- | Cashflow ... in Haskell!! ... with WARP WAI server!!!
module Main where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Blaze.ByteString.Builder.Char8 as BChar8
import qualified Data.ByteString.Char8 as Char8

import qualified Wilde.Application.ApplicationConfiguration as AppConf
import qualified Wilde.Driver.UserInteraction.StandardServiceLinkRenderer as NoResourceRenderer
import Wilde.Driver.Application.Cgi.Wai

import ApplicationConfiguration

import Database.HDBC.MariaDB as Db

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


systemConfiguration :: SystemConfiguration
systemConfiguration = SystemConfiguration
                      {
                        contentEncoder  = BChar8.fromString
                      , queryVarDecoder = Char8.unpack
                      }

warpAppConfig :: AppConf.ApplicationConfiguration
warpAppConfig = appConfig
                {
                  AppConf.standardServiceLinkRenderer = NoResourceRenderer.renderer
                , AppConf.appCssFile                  = Just $ "/html/" ++ ApplicationConfiguration.cssFileName
                }

port = 8081

main :: IO ()
main =
  do
    putStrLn $ "http://localhost:" ++ show port
    Db.withRTSSignalsBlocked $ Warp.run port waiApp

waiApp :: Wai.Application
waiApp = csApplication systemConfiguration warpAppConfig

-- | Cashflow ... in Haskell!! ... with WARP WAI server!!!
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------

import Data.Map as M

import qualified Blaze.ByteString.Builder.Char8 as BChar8
import qualified Data.ByteString.Char8 as Char8

import qualified Data.Text.Encoding as TE

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import qualified Wilde.Application.ApplicationConfiguration as AppConf
import qualified Wilde.Driver.UserInteraction.StandardServiceLinkRenderer as NoResourceRenderer
import qualified Wilde.Driver.Application.WaiServer.Application as WarpApp

import qualified ApplicationConfiguration as TestAppConf

import Database.HDBC.MariaDB as Db


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


systemConfiguration :: WarpApp.SystemConfiguration
systemConfiguration = WarpApp.SystemConfiguration
  {
    WarpApp.contentEncoder  = BChar8.fromString
  , WarpApp.queryVarDecoder = Char8.unpack
  , WarpApp.queryTDecoder   = TE.decodeUtf8
  , WarpApp.contentTEncoder = TE.encodeUtf8Builder
  }

appConfigForServer :: AppConf.ApplicationConfiguration
appConfigForServer = TestAppConf.appConfig
  {
    AppConf.standardServiceLinkRenderer = NoResourceRenderer.renderer
  , AppConf.appCssFile                  = Just $ "/style/" ++ TestAppConf.cssFileName
  }

requestPaths :: WarpApp.PathPrefixesSetup
requestPaths = WarpApp.PathPrefixesSetup
  {
    WarpApp.services = []
  , WarpApp.files    = M.singleton ["style"] "html"
  }

supportedMimeTypes :: WarpApp.MimeTypeMapping
supportedMimeTypes = M.singleton "css" "text/css"

warpAppConf = WarpApp.Configuration
  {
    WarpApp.coding            = systemConfiguration
  , WarpApp.paths             = requestPaths
  , WarpApp.handledMimeTypes  = supportedMimeTypes
  , WarpApp.customPathHandler = Nothing
  }

port = 8081

main :: IO ()
main =
  do
    putStrLn $ "http://localhost:" ++ show port
    Db.withRTSSignalsBlocked $ Warp.run port waiApp

waiApp :: Wai.Application
waiApp = WarpApp.newApplication warpAppConf appConfigForServer

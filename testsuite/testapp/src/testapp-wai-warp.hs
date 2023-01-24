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

import qualified Wilde.Application.ApplicationConfiguration as AppConf
import qualified Wilde.Driver.UserInteraction.StandardServiceLinkRenderer as NoResourceRenderer
import qualified Wilde.Driver.Application.WaiServer.Application as WarpApp
import qualified Wilde.Driver.Application.WaiServer.Cli.Read as CliRead

import qualified ApplicationConfiguration as TestAppConf

import           Database.HDBC.MariaDB as Db
import qualified Wilde.Driver.Application.WaiServer.Cli.WarpMain as WarpMain

import qualified Wilde.Utils.Logging.Class as Logging


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


main :: IO ()
main = WarpMain.main newAppConfigForServer constants cliConfigurable

newAppConfigForServer :: Logging.AnyLogger -> AppConf.ApplicationConfiguration
newAppConfigForServer logger = original
  {
    AppConf.serviceLinks = newSrvcLinks
  , AppConf.appCssFile   = Just $ "/style/" ++ TestAppConf.cssFileName
  }
  where
    original :: AppConf.ApplicationConfiguration
    original = TestAppConf.newAppConfig logger

    newSrvcLinks :: AppConf.ServiceLinks
    newSrvcLinks = (AppConf.serviceLinks original)
      {
        AppConf.standardServiceLinkRenderer = NoResourceRenderer.renderer
      }

constants :: WarpMain.NonConfigurable
constants =
  WarpMain.NonConfigurable
  {
    WarpMain.applicationName     = "Test Application"
  , WarpMain.coding              = codingConfiguration
  , WarpMain.servicesPath        = []
  , WarpMain.fallbackPathHanding = Nothing
  , WarpMain.newLogger           = TestAppConf.newLogger
  , WarpMain.wrapperOfWaiApp     = WarpMain.noWaiWrapping
  , WarpMain.wrapperOfIoAction   = ioWrapper
  }

cliConfigurable :: CliRead.ServerConfiguration
cliConfigurable =
  CliRead.ServerConfiguration
  {
    CliRead.port         = 8081
  , CliRead.fileHandling = filePathsConfig
  , CliRead.logging      = CliRead.Logging
    {
      CliRead.logging_enabled = True
    , CliRead.logging_level   = Logging.LIBRARY
    }
  }

filePathsConfig :: WarpApp.FileHandlingConfiguration
filePathsConfig = WarpApp.FileHandlingConfiguration
  {
    WarpApp.filePaths        = M.singleton ["style"] "html"
  , WarpApp.handledMimeTypes = supportedMimeTypes
  }

codingConfiguration :: WarpApp.CodingConfiguration
codingConfiguration = WarpApp.CodingConfiguration
  {
    WarpApp.contentEncoder  = BChar8.fromString
  , WarpApp.queryVarDecoder = Char8.unpack
  , WarpApp.queryTDecoder   = TE.decodeUtf8
  , WarpApp.contentTEncoder = TE.encodeUtf8Builder
  }

supportedMimeTypes :: WarpApp.MimeTypeMapping
supportedMimeTypes = M.singleton "css" "text/css"

ioWrapper :: CliRead.ServerConfiguration -> IO () -> IO ()
ioWrapper config rawIo = do
    putStrLn $ "http://localhost:" <> show (CliRead.port config)
    Db.withRTSSignalsBlocked rawIo

{-# LANGUAGE OverloadedStrings #-}

module Main where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------

import           Data.Map as M
import qualified System.IO as IO
import qualified Data.Text.IO as TIO

import qualified Database.HDBC.MariaDB as MariaDB

import qualified Wilde.Utils.Logging.Class as Logging
import qualified Wilde.Utils.Logging.SimpleLogger as SimpleLogger
import qualified Wilde.Application.ApplicationConfiguration as AppConf
import qualified Wilde.Driver.Application.WaiServer.Cli.WarpMain as WarpMain
import qualified Wilde.Driver.Application.WaiServer.Application as WarpApp
import qualified Wilde.Driver.Application.WaiServer.Cli.Read as CliRead

import qualified Video.Conf.WaiSystemConfiguration as SysConf
import qualified Video.Conf.AppConf as AppConf


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


main :: IO ()
main  = WarpMain.main newAppConf nonConfigurable cliConfigurable

appName :: String
appName  = "Video"

serverPort :: Int
serverPort  = 8081

newAppConf :: Logging.AnyLogger -> AppConf.ApplicationConfiguration
newAppConf logger = AppConf.newAppConf logger

nonConfigurable :: WarpMain.NonConfigurable
nonConfigurable =
  WarpMain.NonConfigurable
  {
    WarpMain.applicationName     = appName
  , WarpMain.coding              = SysConf.codingConfiguration
  , WarpMain.servicesPath        = []
  , WarpMain.fallbackPathHanding = Nothing
  , WarpMain.newLogger           = newLogger
  , WarpMain.wrapperOfWaiApp     = WarpMain.noWaiWrapping
  , WarpMain.wrapperOfIoAction   = ioWrapper
  }

cliConfigurable :: CliRead.ServerConfiguration
cliConfigurable =
  CliRead.ServerConfiguration
  {
    CliRead.port         = serverPort
  , CliRead.fileHandling = filePathsConfig AppConf.cssFilePath
  , CliRead.logging      = CliRead.Logging
    {
      CliRead.logging_enabled = True
    , CliRead.logging_level   = Logging.LIBRARY
    }
  }

filePathsConfig :: FilePath -> WarpApp.FileHandlingConfiguration
filePathsConfig cssFilePath = WarpApp.FileHandlingConfiguration
  {
    WarpApp.filePaths        = M.fromList
                               [ (AppConf.cssRequestPath_lib, cssFilePath)
                               , (AppConf.cssRequestPath_app, cssFilePath)
                               ]
  , WarpApp.handledMimeTypes = supportedMimeTypes
  }

supportedMimeTypes :: WarpApp.MimeTypeMapping
supportedMimeTypes = M.singleton "css" "text/css"

ioWrapper :: CliRead.ServerConfiguration -> IO () -> IO ()
ioWrapper config rawIo = do
    putStrLn $ "http://localhost:" <> show (CliRead.port config)
    MariaDB.withRTSSignalsBlocked rawIo

newLogger :: Logging.Level -> Logging.AnyLogger
newLogger level = SimpleLogger.newLogger (TIO.hPutStrLn IO.stderr) level mempty

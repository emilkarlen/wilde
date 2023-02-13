-- | Provides a main method that reads CLI args and lanuches
-- a web server.

{-# LANGUAGE StrictData #-}

module Wilde.Driver.Application.WaiServer.Cli.WarpMain
(
   NonConfigurable(..),
    main,

    noIoWrapping,
    noWaiWrapping,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.ByteString as B
import System.Exit as SysExit

import qualified Data.Yaml as Yaml

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import qualified Wilde.Application.ApplicationConfiguration as AppConf

import qualified Wilde.Driver.Application.Web.Types as AppTypes
import qualified Wilde.Driver.Application.WaiServer.RequestHandling.Main.Types as Types
import qualified Wilde.Driver.Application.WaiServer.RequestHandling.Types as RqH
import qualified Wilde.Driver.Application.WaiServer.Cli.Read as CliRead
import           Wilde.Driver.Application.WaiServer.ConfigFile.Configuration as ConfigFileConf
import           Wilde.Driver.Application.WaiServer.ConfigFile.Yaml ()
import qualified Wilde.Driver.Application.WaiServer.Application as WarpApp

import qualified Wilde.Utils.Logging.Class as Logger
import qualified Wilde.Utils.Logging.NoLogging as NoLogging
import qualified Wilde.Driver.Application.WaiServer.Cli.Help as Help


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Things that cannot be configured via the CLI or configuration files.
data NonConfigurable =
   NonConfigurable
   {
      applicationName     :: String
      -- ^ The name of the application, to display in help text.
      -- See output of --help for details.
   ,  coding              :: AppTypes.CodingConfiguration
   ,  servicesPath        :: AppTypes.RequestPath
   ,  fallbackPathHanding :: Maybe RqH.RequestHandlerResolver
   ,  newLogger           :: Logger.Level -> Logger.AnyLogger
      -- ^ Gives a logger for a given logging level.
   ,  wrapperOfWaiApp     :: Wai.Application -> Wai.Application
      -- ^ Wrapping of the WAI application
      -- using a Wai "middleware".
      --
      -- Use `noWaiWrapping` if not needed.
   ,  wrapperOfIoAction   :: CliRead.ServerConfiguration -> IO () -> IO ()
      -- ^ wrapping of the warp server IO action.
      -- The first argument is the configuration that will be used,
      -- i.e. the one resolved from the defaults and the CLI arguments
      -- and configuration files.
      --
      -- Use `noIoWrapping` if not needed.
   }

-- | No wrapping of IO,
-- for use with `main`.
noIoWrapping :: CliRead.Configuration -> IO () -> IO ()
noIoWrapping _ io = io

-- | No wrapping of the WAI application,
-- for use with `main`.
noWaiWrapping :: Wai.Application -> Wai.Application
noWaiWrapping = id

main
   :: (Logger.AnyLogger -> AppConf.ApplicationConfiguration)
   -> NonConfigurable
   -> CliRead.ServerConfiguration
      -- ^ defaults - may be overrided via CLI arguments
      -- and configuration files
   -> IO ()
main mkAppConf constants defaults =
   do
      config <- CliRead.exec (applicationName constants) defaults
      CliRead.eliminate doPrintDefaults doConfigurationHelp doServer config
   where
      doPrintDefaults :: IO ()
      doPrintDefaults = do
         printDefaultsAsYaml defaults
         SysExit.exitSuccess

      doConfigurationHelp :: IO ()
      doConfigurationHelp = do
         Help.printConfigurationHelp
         SysExit.exitSuccess

      doServer :: CliRead.ServerConfiguration -> IO ()
      doServer = server mkAppConf constants

server
   :: (Logger.AnyLogger -> AppConf.ApplicationConfiguration)
   -> NonConfigurable
   -> CliRead.ServerConfiguration
   -> IO ()
server mkAppConf constants serverConf =
   getIoAction serverConf (getWaiApp serverConf)
   where
      getServerConf :: CliRead.ServerConfiguration -> WarpApp.MainConfiguration
      getServerConf config =
         WarpApp.MainConfiguration
           {
             WarpApp.coding       = coding constants
           , WarpApp.requestPaths =
               WarpApp.RequestPathsConfiguration
               {
                 WarpApp.services = servicesPath constants
               , WarpApp.files    = CliRead.fileHandling config
               , WarpApp.fallback = fallbackPathHanding constants
               }
           }

      appConf :: AppConf.ApplicationConfiguration
      appConf = mkAppConf $ getLogger $ CliRead.logging serverConf

      getLogger :: CliRead.Logging -> Logger.AnyLogger
      getLogger (CliRead.Logging False _)    = NoLogging.theLogger
      getLogger (CliRead.Logging True level) = newLogger constants level

      getWaiApp :: CliRead.ServerConfiguration -> Wai.Application
      getWaiApp config =
         let
            rawWaiApp = WarpApp.newApplication (getServerConf config) appConf
         in
            wrapperOfWaiApp constants rawWaiApp

      getIoAction :: CliRead.ServerConfiguration -> Wai.Application -> IO ()
      getIoAction config waiApp =
         let
            port = CliRead.port config
            rawWarpAction = Warp.run port waiApp
         in
            wrapperOfIoAction constants config rawWarpAction

printDefaultsAsYaml :: CliRead.ServerConfiguration -> IO ()
printDefaultsAsYaml defaults = do
   B.putStr byteString
   SysExit.exitSuccess
   where
      byteString :: B.ByteString
      byteString = Yaml.encode $ asCliConfiguration defaults

asCliConfiguration :: CliRead.ServerConfiguration -> ConfigFileConf.Configuration
asCliConfiguration serverConf =
   ConfigFileConf.Configuration
   {
      port  = Just $ CliRead.port serverConf
    , files = Just $ ConfigFileConf.Files
         {
            file_paths = Just $ Types.filePaths fileHandling
         ,  mime_types = Just $ Types.handledMimeTypes fileHandling
         }
   ,  logging = Just $ logging_info $ CliRead.logging serverConf
   }
   where
      fileHandling :: Types.FileHandlingConfiguration
      fileHandling = CliRead.fileHandling serverConf

      logging_info :: CliRead.Logging -> ConfigFileConf.Logging
      logging_info (CliRead.Logging enabled level) =
         ConfigFileConf.Logging
         {
            ConfigFileConf.enabled = Just enabled
         ,  ConfigFileConf.level   = Just level
         }

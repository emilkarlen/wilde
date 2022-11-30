-- | Creation of a web server using warp.
module Wilde.Driver.Application.WaiServer.Application
       (
         -- * Construction of a WAI Application

         ContentEncoder,
         ContentDecoder,
         TextEncoder,
         TextDecoder,

         SystemConfiguration(..),

         newApplication,

         -- * Re-exported from "Wilde.Application.ApplicationConfiguration"

         MainHandler.Configuration(..),

         RTR.PathPrefixesSetup(..),
         FileTypes.MimeTypeMapping,
         -- AppConf.ApplicationConfiguration(..),
         -- AppConf.ApplicationServices,
         -- AppConf.Translations(..),
         )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Network.Wai as Wai

import Wilde.Driver.Application.Types

import           Wilde.Application.ApplicationConfiguration as AppConf

import qualified Wilde.Driver.Application.WaiServer.RequestHandling.Main.RequestTypeResolving as RTR
import qualified Wilde.Driver.Application.WaiServer.RequestHandling.File.Types as FileTypes
import qualified Wilde.Driver.Application.WaiServer.RequestHandling.Main.Handler as MainHandler


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


newApplication :: MainHandler.Configuration
               -> AppConf.ApplicationConfiguration
               -> Wai.Application
newApplication = MainHandler.newApplication

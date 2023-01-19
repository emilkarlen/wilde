-- | Creation of a web server using warp.
module Wilde.Driver.Application.WaiServer.Application
       (
         -- * Construction of a WAI Application

         ContentEncoder,
         ContentDecoder,
         TextEncoder,
         TextDecoder,

         CodingConfiguration(..),

         newApplication,

         -- * Re-exported from "Wilde.Application.ApplicationConfiguration"

         MainHandler.MainConfiguration(..),
         MainHandler.RequestPathsConfiguration(..),
         MainHandler.FileHandlingConfiguration(..),

         FileTypes.MimeTypeMapping,
         )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Network.Wai as Wai

import           Wilde.Driver.Application.Types

import           Wilde.Application.ApplicationConfiguration as AppConf

import qualified Wilde.Driver.Application.WaiServer.RequestHandling.File.Types as FileTypes
import qualified Wilde.Driver.Application.WaiServer.RequestHandling.Main.Handler as MainHandler


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


newApplication :: MainHandler.MainConfiguration
               -> AppConf.ApplicationConfiguration
               -> Wai.Application
newApplication = MainHandler.newApplication

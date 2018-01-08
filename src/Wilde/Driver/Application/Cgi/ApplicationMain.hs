{-
Copyright 2013 Emil Karlén.

This file is part of Wilde.

Wilde is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Wilde is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Wilde.  If not, see <http://www.gnu.org/licenses/>.
-}

-- | A main function for CGI programs.
module Wilde.Driver.Application.Cgi.ApplicationMain
       (
         -- * Construction of a CGI Application

         wildeMain,
         
         -- * Re-exported from "Wilde.Application.ApplicationConfiguration"

         ApplicationServices,
         ApplicationConfiguration(..),
         Translations(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Control.Monad.Trans

import Network.CGI

import qualified Text.Html as THtml

import Wilde.GenericUi.Components

import Wilde.WildeUi.StdValueTypes

import           Wilde.Media.WildeStyle (errorStyle)

import           Wilde.Render.RenderAsHtml
import qualified Wilde.Render.Cgi.ElementSetIo as ElementSetIo

import           Wilde.Application.ApplicationConfiguration as AppConf

import qualified Wilde.Driver.Application.CgiHtml as AppCgiHtml


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | The "main" function of an application, in terms of CGI.
cgiMain :: ApplicationConfiguration -> CGI CGIResult
cgiMain appConf =
  do
    cgiInput           <- getInputs
    let rawRequestInput = map ElementSetIo.toOptionalValue cgiInput
    result             <- liftIO $ AppCgiHtml.lookupAndRunService appConf rawRequestInput
    case result of
      Left err         -> topLevelError appConf err
      Right htmlString -> output htmlString

topLevelError :: AppConf.ApplicationConfiguration
              -> AppCgiHtml.ServiceSpecificationError 
              -> CGI CGIResult
topLevelError appConf err = output $ THtml.renderHtml html
  where
    mbCssFile  = AppConf.appCssFile appConf
    tr         = AppConf.translations appConf
    html       = renderPage mbCssFile title components
    title      = withWildeStyle errorStyle (trErorPageTitle tr)
    components = [anyValueComponent (UnquotedStringValue (show err))]

-- | Main function for applications using "Network.CGI". 
wildeMain :: ApplicationConfiguration -> IO ()
wildeMain config =
  runCGI $ handleErrors (cgiMain config)

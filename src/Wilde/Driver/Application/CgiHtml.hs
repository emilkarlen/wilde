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

-- Utilities for Appliation Drivers that use CGI _and_ HTML.
module Wilde.Driver.Application.CgiHtml
       (
         lookupAndRunService,
         AppCgi.ServiceSpecificationError(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Render.Cgi.ElementSetIo as ElementSetIo

import qualified Wilde.Driver.Application.Cgi as AppCgi
import qualified Wilde.Driver.Application.Html as AppHtml

import qualified Wilde.Application.ApplicationInput as AppInput
import qualified Wilde.Application.ApplicationConfiguration as AppConf


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Combines 'AppCgi.getServiceAndEnvironment'
-- and 'AppHtml.runService_htmlString'.
lookupAndRunService :: AppConf.ApplicationConfiguration
                    -> ElementSetIo.ServerVariables
                    -> IO (Either AppCgi.ServiceSpecificationError String)
lookupAndRunService
  appConf@(
    AppConf.ApplicationConfiguration
    {
      AppConf.translations = theTranslations
    , AppConf.appCssFile   = theAppCssFile
    })
  rawRequestInput =
      case AppCgi.getServiceAndEnvironment appConf requestInput of
        Left err -> return (Left err)
        Right (service,environment) -> do
          htmlString <- AppHtml.runService_htmlString
                        theAppCssFile 
                        theTranslations 
                        environment 
                        service
          return (Right htmlString)
  where
    requestInput :: AppInput.Input
    requestInput = ElementSetIo.inputFromCgiValues rawRequestInput

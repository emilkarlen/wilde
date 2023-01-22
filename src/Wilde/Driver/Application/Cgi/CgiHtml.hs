-- Utilities for Appliation Drivers that use CGI _and_ HTML.
module Wilde.Driver.Application.Cgi.CgiHtml
       (
         lookupAndRunService,
         ServiceLookup.ServiceSpecificationError(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Render.Cgi.ElementSetIo as ElementSetIo

import qualified Wilde.Driver.Application.Cgi.ServiceLookup as ServiceLookup
import qualified Wilde.Driver.Application.Html as AppHtml

import qualified Wilde.Application.ApplicationInput as AppInput
import qualified Wilde.Application.ApplicationConfiguration as AppConf


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Combines 'ServiceLookup.getServiceAndEnvironment'
-- and 'AppHtml.runService_htmlString'.
lookupAndRunService :: AppConf.ApplicationConfiguration
                    -> ElementSetIo.ServerVariables
                    -> IO (Either ServiceLookup.ServiceSpecificationError String)
lookupAndRunService
  appConf@(
    AppConf.ApplicationConfiguration
    {
      AppConf.translations = theTranslations
    , AppConf.appCssFile   = theAppCssFile
    })
  rawRequestInput =
      case ServiceLookup.getServiceAndEnvironment appConf requestInput of
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

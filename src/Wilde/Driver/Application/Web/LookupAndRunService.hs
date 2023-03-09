-- Utilities for Appliation Drivers that use CGI _and_ HTML.
module Wilde.Driver.Application.Web.LookupAndRunService
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
import qualified Wilde.Driver.Application.Web.RunService as AppHtml

import qualified Wilde.Application.ApplicationInput as AppInput
import qualified Wilde.Application.ApplicationConfiguration as AppConf


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Combines 'ServiceLookup.getServiceAndEnvironment'
-- and 'AppHtml.runService_htmlString'.
lookupAndRunService :: AppConf.ApplicationConfiguration
                    -> ElementSetIo.ServerVariables
                    -> IO (Either ServiceLookup.ServiceSpecificationError AppHtml.HtmlAsString)
lookupAndRunService
  appConf@(
    AppConf.ApplicationConfiguration
    {
      AppConf.translations = theTranslations
    , AppConf.appCssFiles   = theAppCssFile
    })
  rawRequestInput =
      case ServiceLookup.getServiceAndEnvironment appConf requestInput of
        Left err -> pure (Left err)
        Right (service,environment) -> do
          htmlString <- AppHtml.runService_htmlString
                        theAppCssFile
                        theTranslations
                        environment
                        service
          pure (Right htmlString)
  where
    requestInput :: AppInput.Input
    requestInput = ElementSetIo.inputFromCgiValues rawRequestInput

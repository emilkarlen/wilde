-- | Defines sdfsdfsdf
module Wilde.Driver.Application.Cgi.CustomServiceLinkRenderer where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Media.Presentation as Presentation
import qualified Wilde.Media.WildeValue as WildeValue
import qualified Wilde.Media.ElementSet as ES
import Wilde.Application.ServiceLink (
    GenericParameter, ServiceLink(..),
    ServiceSpecification,
    ServiceReferenceWithParams(..),
    )
import Wilde.Media.WildeStyleType (WildeStyling)
import Wilde.WildeUi.StdValueTypes (LinkLabel, wwwLinkValue_dr)
import Wilde.Driver.Application.Cgi.ServiceLink (href)

-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


getCustomServiceLinkRenderer :: Presentation.Monad CustomServiceLinkRenderer
getCustomServiceLinkRenderer = do
    custEnv <- Presentation.getCustomEnvironment
    pure $ customServiceLinkRenderer custEnv


customServiceLinkRenderer :: ES.ElementSet        -- ^ Custom environment
                          -> ServiceSpecification -- ^ The service
                          -> WildeStyling LinkLabel -- ^ Link label
                          -> [GenericParameter]   -- ^ Parameters that should
                                                  -- be passed to the service.
                          -> WildeValue.AnySVALUE
customServiceLinkRenderer custEnv srvcSpec label params =
  WildeValue.AnySVALUE wwwLink
  where
    wwwLink        = wwwLinkValue_dr srvcHref label
    srvcHref       = href srvcLink
    srvcLink       = ServiceLink srvcRefWParams custEnv
    srvcRefWParams = ServiceReferenceWithParams srvcSpec params 

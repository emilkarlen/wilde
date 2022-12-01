-- | Defines sdfsdfsdf
module Wilde.Driver.Application.Cgi.ServiceLinkRenderers
(
  getMkStandardObjectTypeServiceLink,
  getMkStandardObjectServiceLink,
  getMkGenericServiceLink,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Media.Presentation as Presentation
import qualified Wilde.Media.WildeValue as WildeValue
import qualified Wilde.Media.ElementSet as ES
import Wilde.Media.WildeStyleType (WildeStyling)
import Wilde.WildeUi.StdValueTypes (LinkLabel, wwwLinkValue_dr)
import Wilde.Driver.Application.Cgi.ServiceLink (href)
import Wilde.Application.StandardServiceLinks
import Wilde.Service.ServiceLink


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------

getMkStandardObjectTypeServiceLink :: Presentation.Monad MkObjectTypeServiceLink
getMkStandardObjectTypeServiceLink = do
    custEnv <- Presentation.getCustomEnvironment
    pure $ mkLink custEnv
    where
      mkLink :: ES.ElementSet -> MkObjectTypeServiceLink
      mkLink custEnv serviceEnum label objectTypeId params =
        serviceLinkRenderer custEnv srvcSpec label params
        where
          srvcSpec = newObjectTypeServiceSpecification (show serviceEnum) objectTypeId


getMkStandardObjectServiceLink :: Presentation.Monad MkObjectServiceLink
getMkStandardObjectServiceLink = do
    custEnv <- Presentation.getCustomEnvironment
    pure $ mkLink custEnv
    where
      mkLink :: ES.ElementSet -> MkObjectServiceLink
      mkLink custEnv serviceEnum label objectTypeId objectId params =
        serviceLinkRenderer custEnv srvcSpec label params
        where
          srvcSpec = newObjectServiceSpecification (show serviceEnum) objectTypeId objectId

getMkGenericServiceLink :: Presentation.Monad Presentation.MkGenericServiceLink
getMkGenericServiceLink = do
    custEnv <- Presentation.getCustomEnvironment
    pure $ serviceLinkRenderer custEnv

serviceLinkRenderer :: ES.ElementSet          -- ^ Custom environment
                    -> ServiceSpecification   -- ^ The service
                    -> WildeStyling LinkLabel -- ^ Link label
                    -> [GenericParameter]     -- ^ Parameters that should
                                              -- be passed to the service.
                    -> WildeValue.AnySVALUE
serviceLinkRenderer custEnv srvcSpec label params =
  WildeValue.AnySVALUE wwwLink
  where
    wwwLink        = wwwLinkValue_dr srvcHref label
    srvcHref       = href srvcLink
    srvcLink       = ServiceLink srvcRefWParams custEnv
    srvcRefWParams = ServiceReferenceWithParams srvcSpec params

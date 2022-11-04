module Wilde.Driver.Application.Cgi.ServiceLink where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.Maybe


import           Wilde.WildeUi.StdValueTypes

import qualified Wilde.Driver.Application.Cgi.VariableNames as VariableNames
import qualified Wilde.Render.Cgi.ElementSetIo as ElmSetIo

import           Wilde.Application.ServiceLink


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Gives a `HRef` for a given `ServiceLink`.
href :: ServiceLink -> HRef
href serviceLink = HRef "" cgiParams
  where
    cgiParams = toCgiParams serviceLink

cgiParamsForServiceSpec :: ServiceSpecification -> ElmSetIo.ServerVariables
cgiParamsForServiceSpec serviceSpec =
  (VariableNames.service,Just $ ssServiceName serviceSpec) :
  catMaybes [ withLabel VariableNames.objectType  ssObjectTypeName
            , withLabel VariableNames.pk          ssObjectIdentity
            ]
  where
    withLabel label getMbVal = (\x -> (label,Just x)) <$> getMbVal serviceSpec

toCgiParams :: ServiceLink -> ElmSetIo.ServerVariables
toCgiParams serviceLink =
  cgiParamsForServiceSpec serviceSpecification
  ++
  map ElmSetIo.toOptionalValue (srwpGenericParams serviceReferenceWithParams)
  ++
  ElmSetIo.customEnvironmentSetToCgiValues (slCustomEnvironment serviceLink)
  where
    withLabel label getMbVal = fmap (\x -> (label,Just x)) $ getMbVal serviceSpecification
    serviceReferenceWithParams = slServiceReferenceWithParams
                                 serviceLink
    serviceSpecification = srwpServiceSpecification serviceReferenceWithParams

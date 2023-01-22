-- | Some utilities for tests that operates on a single Attribute Type.
module WildeTest.ObjectModel.UserInteraction.SingleAttributeObjectModelTestResources
       (
         AtValueType,
         AtDefaultType,

         atTitle,

         theObjectName,
         otherObjectName,
         theAttributeName,

         theAttributeElementKey,

         theFix_env_value,
         theDefault_app_value,
         theDefault_env_value,

         metaValuesForFixedGsr,
         metaValuesForFixedGsr_forObject,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Media.ElementSet as ES
import qualified Wilde.Media.UserInteraction as UserInteraction
import qualified Wilde.ObjectModel.UserInteraction.Common as UserInteractionCommon


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type AtValueType   = String

type AtDefaultType = String


-- | Name of the object used by all tests.
theObjectName :: UserInteraction.ObjectName
theObjectName = UserInteraction.objectName "object-name"

otherObjectName :: UserInteraction.ObjectName
otherObjectName = UserInteraction.objectName "other-object-name"

-- | Name of the attribute used by all tests.
theAttributeName :: UserInteraction.AttributeName
theAttributeName = UserInteraction.attributeName "attribute-name"

-- | Element key of the attribute used by all tests.
theAttributeElementKey :: ES.ElementKey
theAttributeElementKey = UserInteraction.attributeElementKey theObjectName theAttributeName

atTitle :: String
atTitle = "Attribute Title"

-- | Fix value from the Environment used by all tests.
theFix_env_value :: AtValueType
theFix_env_value = "FIX-FROM-ENV"

-- | Default value from the Application used by all tests.
theDefault_app_value :: AtDefaultType
theDefault_app_value = "DEFAULT-FROM-APP"

-- | Default value from the Environment used by all tests.
theDefault_env_value :: AtDefaultType
theDefault_env_value = "DEFAULT-FROM-ENV"

metaValuesForFixedGsr_forObject :: UserInteraction.ObjectName -> AtValueType -> [ES.Element]
metaValuesForFixedGsr_forObject objectName =
  UserInteractionCommon.metaValuesForRole UserInteractionCommon.Fix
  theAttributeName
  objectName

metaValuesForFixedGsr :: AtValueType -> [ES.Element]
metaValuesForFixedGsr =  metaValuesForFixedGsr_forObject theObjectName

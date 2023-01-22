{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

-------------------------------------------------------------------------------
-- | Defines functionality for User Interaction related to 'ObjectType's.
--
-- Names here do not include \"UserInteraction\" - this assumed.
-------------------------------------------------------------------------------
module Wilde.ObjectModel.UserInteraction
       (
         -- * Classes for AttributeType

         -- ** Create

         ATTRIBUTE_FOR_CREATE(..),

         AttributeTypeCreateOption(..),
         UserInteractionCreateDefault,

         -- *** IO

         ATTRIBUTE_INPUT_FOR_CREATE(..),

         ATTRIBUTE_OUTPUT_FOR_CREATE(..),

         ATTRIBUTE_IO_FOR_CREATE(..),

         -- ** Existing

         -- *** IO

         ATTRIBUTE_OUTPUT_FOR_EXISTING(..),

         ATTRIBUTE_INPUT_FOR_EXISTING(..),
         AttributeInputer,

         ATTRIBUTE_IO_FOR_EXISTING(..),

         -- * Types

         module Wilde.ObjectModel.UserInteraction.OutputTypes,

         -- * Special 'Element' values

         elementValueEncodeObjectName,
         elementValueDecodeObjectName,
         elementValueEncodeObjectNameList,
         elementValueDecodeObjectNameList,

         -- * Tools

         ObjectInputResult,

         otUiObjectInputErrorInfo,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Utils.NonEmptyList as NonEmpty

import qualified Wilde.Media.ElementSet as ES
import Wilde.Media.WildeMedia hiding (otKey)
import Wilde.Media.GenericStringRep
import Wilde.Media.UserInteraction
import Wilde.Media.UserInteraction.Input
import Wilde.Media.UserInteraction as UI

import           Wilde.ObjectModel.ObjectModelUtils as OmUtils
import qualified Wilde.ObjectModel.GenericStringRep as OmGsr
import Wilde.ObjectModel.Presentation (ATTRIBUTE_PRESENTATION(..))

import Wilde.ObjectModel.UserInteraction.OutputTypes


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Functionality of an 'AttributeType' required by both Input and Output
-- for creating an 'Object'.
class ATTRIBUTE_FOR_CREATE atConf where
  atCreateOption :: AttributeType atConf dbTable typeForExisting typeForCreate
                 -> Maybe (AttributeTypeCreateOption typeForCreate)

-- | Alternative ways to supply a value for creating an 'Object'.
data AttributeTypeCreateOption typeForCreate
     = AtuicoFixed   typeForCreate -- ^ A value that the user cannot modify.
       -- | A value that is prefilled in the UI
     | AtuicoDefault (UserInteractionCreateDefault typeForCreate)

-- | Specifies a default value for UI when creating an 'Object'.
type UserInteractionCreateDefault typeForCreate = Either UI.GenericWidgetDefaultValue typeForCreate


-------------------------------------------------------------------------------
-- - create/input -
-------------------------------------------------------------------------------


-- | Class for extracting 'AttributeTypeInfo' from the annotation of an
-- 'AttributeType'.
class (OmGsr.ATTRIBUTE_INPUT_FOR_CREATE atConf
      ,ATTRIBUTE_FOR_CREATE atConf
      )
      =>
      ATTRIBUTE_INPUT_FOR_CREATE atConf where
        atInputerForCreate :: AttributeType atConf dbTable typeForExisting typeForCreate
                           -> AttributeName -> UserInteractionInputer (ES.ElementInputResult typeForCreate)


-------------------------------------------------------------------------------
-- - create/output -
-------------------------------------------------------------------------------


-- | Class for extracting 'AttributeTypeInfo' from the annotation of an
-- 'AttributeType'.
class (ATTRIBUTE_PRESENTATION atConf
       ,OmGsr.ATTRIBUTE_IO_FOR_CREATE atConf
       ,ATTRIBUTE_FOR_CREATE atConf
       )
       => ATTRIBUTE_OUTPUT_FOR_CREATE atConf where
   atOutputerForCreate :: AttributeType atConf dbTable typeForExisting typeForCreate
                       -> AttributeName -> AttributeTypeOutputerForCreate typeForExisting typeForCreate


-------------------------------------------------------------------------------
-- - create/io -
-------------------------------------------------------------------------------


class (ATTRIBUTE_INPUT_FOR_CREATE atConf
       ,ATTRIBUTE_OUTPUT_FOR_CREATE atConf
       )
       => ATTRIBUTE_IO_FOR_CREATE atConf where


-------------------------------------------------------------------------------
-- - existing/input -
-------------------------------------------------------------------------------


-- | Class for extracting 'AttributeTypeInfo' from the annotation of an
-- 'AttributeType'.
class OmGsr.ATTRIBUTE_INPUT_FOR_EXISTING atConf
      =>
      ATTRIBUTE_INPUT_FOR_EXISTING atConf where
  atInputerForExisting :: AttributeType atConf dbTable typeForExisting typeForCreate ->
                          AttributeInputer typeForExisting

type AttributeInputer a = AttributeName ->
                          UserInteractionInputer (ES.ElementInputResult a)


-------------------------------------------------------------------------------
-- - existing/output -
-------------------------------------------------------------------------------


-- | Class for extracting 'AttributeTypeInfo' from the annotation of an
-- 'AttributeType'.
class ATTRIBUTE_PRESENTATION atConf => ATTRIBUTE_OUTPUT_FOR_EXISTING atConf where
  atOutputerForExisting :: AttributeType atConf dbTable typeForExisting typeForCreate
                        -> UserInteractionOutputerForExisting typeForExisting


-------------------------------------------------------------------------------
-- - existing/io -
-------------------------------------------------------------------------------


class (ATTRIBUTE_INPUT_FOR_EXISTING atConf
      ,ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
       )
      =>
      ATTRIBUTE_IO_FOR_EXISTING atConf where


-------------------------------------------------------------------------------
-- - Default values -
-------------------------------------------------------------------------------


-- | The result of inputting an 'Object' from the User Interaction Input
-- Media.
type ObjectInputResult a = Either ObjectInputErrorInfo a

-- | Helper method for constructing a 'ObjectInputErrorInfo'.
otUiObjectInputErrorInfo :: CrossRefIdentifier -- ^ Object Type ID
                         -> ObjectName -- ^ Object Name
                         -> NonEmpty.List ES.ElementLookupError
                         -> ObjectInputErrorInfo
otUiObjectInputErrorInfo otKey objectName attrErrorInfos =
    ObjectInputErrorInfo otKey (show objectName) attrErrorInfos


-------------------------------------------------------------------------------
-- - outputerExisting -
-------------------------------------------------------------------------------


-- | Hides the type of a 'MkAttributeOutputForExisting'.
data MkAttributeOutputForExistingAny
     = forall typeFromAttributeType . Typeable typeFromAttributeType =>
       MkAttributeOutputForExistingAny (MkAttributeOutputForExisting typeFromAttributeType)

-- | A function that produces 'LabelAndWidget' for the native value of an 'Attribute'.
type MkAttributeOutputForExisting typeFromAttributeType = typeFromAttributeType -> ObjectName -> LabelAndWidget

lookupFixedAttributeGenericStringRep :: ElementKey
                                     -> ES.Lookuper (ES.ElementInputResult
                                                     (ElementKey,
                                                      Maybe GenericStringRep))
lookupFixedAttributeGenericStringRep ek set =
  return $ either Left (\mbValue -> Right (ek,mbValue)) mbValueR
  where
    mbValueR = ES.lookupSingleton_optional ek set


-------------------------------------------------------------------------------
-- - Element Values -
-------------------------------------------------------------------------------


elementValueEncodeObjectName :: ObjectName -> ElementValue
elementValueEncodeObjectName = elementValueEncodeElementKeyPrefix

elementValueDecodeObjectName :: ElementValue -> ObjectName
elementValueDecodeObjectName = elementValueDecodeElementKeyPrefix

elementValueEncodeObjectNameList :: [ObjectName] -> ElementValue
elementValueEncodeObjectNameList = elementValueEncodeList . map elementValueEncodeObjectName

elementValueDecodeObjectNameList :: ElementValue -> [ObjectName]
elementValueDecodeObjectNameList = map elementValueDecodeObjectName . elementValueDecodeList

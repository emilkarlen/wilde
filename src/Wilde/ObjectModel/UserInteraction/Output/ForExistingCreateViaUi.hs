-- | Not sure what this outputer does.
module Wilde.ObjectModel.UserInteraction.Output.ForExistingCreateViaUi
       (
         AttributeTypeInfo(..),
         ATTRIBUTE_OUTPUT_FOR_EXISTING(..),

         AttributeInfo(..),

         outputer,

         -- * Re-exporting

         module Wilde.ObjectModel.UserInteraction.Output.ExistingCommon,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Utils.AnyValue as AnyValue

import           Wilde.Media.UserInteraction
import           Wilde.Media.UserInteraction.Output

import           Wilde.ObjectModel.ObjectModelUtils as OmUtils
import qualified Wilde.ObjectModel.Presentation as OmPres
import           Wilde.ObjectModel.UserInteraction.Output.ExistingCommon


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Gives an outputer for an 'Object', that is \"created\" via the UI.
-------------------------------------------------------------------------------
outputer :: (ATTRIBUTE_OUTPUT_FOR_EXISTING atConf)
         => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
         -> [Any (AttributeType atConf dbTable)]
         -- ^ Specifies the order in which
         -- the 'AttributeType's inputers are listed.
         -- The list must be a permutation of all 'AttributeType's of the
         -- 'ObjectType'.
         -> UserInteractionOutputMonad (ObjectName -> FormBlock)
outputer ot attributeTypesOrder =
  do
    mkAttrOutputList <- sequence mkAttrOutputFunList
    pure $ getObjectTypeOutput mkAttrOutputList
  where
    attributeTypeInfosOrder = map
                              (OmUtils.anyValueApply (AnyValue.Container . at2ati))
                              attributeTypesOrder

    getObjectTypeOutput :: [ObjectName -> FormBlockRow] -> ObjectName -> FormBlock
    getObjectTypeOutput mkAttrOutputFunList objectName =
      FormBlock attributeOutputList [] (OmPres.objectTypeStyle ot)
      where
        attributeOutputList = map
                              (\mkAttrOutputForObj -> mkAttrOutputForObj objectName)
                              mkAttrOutputFunList

    mkAttrOutputFunList = map
                          (AnyValue.apply getAttrOutput)
                          attributeTypeInfosOrder

-------------------------------------------------------------------------------
-- Output, for an existing object that is created via UI, of a single
-- 'AttributeTypeInfo'.
-------------------------------------------------------------------------------
getAttrOutput :: AttributeTypeInfo typeForExisting
              -> UserInteractionOutputMonad (ObjectName
                                             -> FormBlockRow)
getAttrOutput ati =
   do
     mkAttributeOutput <- getMkAttributeOutputFun (UserInteraction,ati)
     pure $ mkAttributeOutput Nothing

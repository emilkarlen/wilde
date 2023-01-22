{-# LANGUAGE Rank2Types #-}

-- | Construction of outputers for User Interaction.
--
-- Outputer of a form for entering an 'Object', with defaults
-- from an existing 'Object'.
module Wilde.ObjectModel.UserInteraction.Output.ForCreateFrom
       (
         AttributeTypeInfo(..),
         ATTRIBUTE_OUTPUT_FOR_CREATE(..),

         --outputerOt,
         outputer,
         -- outputerNoClass,

         AttributeWidgetDefaultValueForCreate(..),
         AttributeTypeOutputerForCreate,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Media.UserInteraction
import Wilde.Media.UserInteraction.Output

import           Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.Presentation as OmPres
import qualified Wilde.ObjectModel.ObjectModelUtils as OmUtils
import           Wilde.ObjectModel.UserInteraction.Output.CreateCommon


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


outputer :: ATTRIBUTE_OUTPUT_FOR_CREATE atConf
         => [OmUtils.Any (AttributeType atConf dbTable)]
         -- ^ Specifies the order in which
         -- the 'AttributeType's inputers are listed.
         -- The list must be a permutation of all 'AttributeType's of the
         -- 'ObjectType'.
         -> Object otConf atConf dbTable otNative idAtExisting idAtCreate
         -> UserInteractionOutputMonad (ObjectName -> FormBlock)
outputer = outputerNoClass at2ati


outputerNoClass :: (forall e c . AttributeType atConf dbTable e c
                    -> AttributeTypeInfo e c)
                -> [OmUtils.Any (AttributeType atConf dbTable)]
                -- ^ Specifies the order in which
                -- the 'AttributeType's inputers are listed.
                -- The list must be a permutation of all 'AttributeType's of the
                -- 'ObjectType'.
                -> Object otConf atConf dbTable otNative idAtExisting idAtCreate
                -> UserInteractionOutputMonad (ObjectName -> FormBlock)
outputerNoClass at2ati attributeTypesOrder o@(Object {}) =
  do
     attributeList         <- toUserInteractionOutputMonad $
                              OmPres.getAttributesInGivenOrder attributeTypesOrder o
     let mkAttrOutputerList = map (getMkAttrOutputer at2ati) attributeList
     attrOutputerList      <- sequence mkAttrOutputerList
     pure $ getObjectTypeOutput attrOutputerList
  where
    getMkAttrOutputer :: (forall e c . AttributeType atConf dbTable e c
                          -> AttributeTypeInfo e c)
                      -> Any (Attribute atConf dbTable)
                      -> UserInteractionOutputMonad (ObjectName -> FormBlockRowInfo)
    getMkAttrOutputer at2ati (Any attr) =
      do
        let ati = (at2ati . attrType) attr
        outputer <- atiOutputerForCreate ati (atiCrossRefKey ati)
        let defaultValue = Just $ DefaultCreateFromExisting (attrValue attr)
        pure $
          \objectName -> mkFormBlockRowInfoForWidget
                         (atiCrossRefKey ati)
                         (atiTitle ati)
                         outputer
                         defaultValue
                         objectName

    getObjectTypeOutput :: [ObjectName -> FormBlockRowInfo]
                        -> ObjectName
                        -> FormBlock
    -- TODO Improve handling of empty list of attributes to input.
    getObjectTypeOutput [] objectName = FormBlock [] []

    getObjectTypeOutput attrOutputerList objectName =
      let
        rowInfos = [attrOutputer objectName | attrOutputer <- attrOutputerList]
      in
       concatAtFormBlockInfos rowInfos

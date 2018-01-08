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
     return $ getObjectTypeOutput attrOutputerList
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
        return $ 
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

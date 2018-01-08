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

import Wilde.Media.UserInteraction
import Wilde.Media.UserInteraction.Output

import Wilde.ObjectModel.ObjectModelUtils as OmUtils

import Wilde.ObjectModel.UserInteraction.Output.ExistingCommon


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
    return $ getObjectTypeOutput mkAttrOutputList
  where
    attributeTypeInfosOrder = map
                              (OmUtils.anyValueApply (AnyValue.Container . at2ati))
                              attributeTypesOrder
    
    getObjectTypeOutput :: [ObjectName -> FormBlockRow] -> ObjectName -> FormBlock
    getObjectTypeOutput mkAttrOutputFunList objectName =
      FormBlock attributeOutputList []
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
     return $ mkAttributeOutput Nothing

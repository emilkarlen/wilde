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

{-# LANGUAGE ExistentialQuantification #-}

-- | Construction of outputers that lets the user enter
-- an existing 'Object'.
module Wilde.ObjectModel.UserInteraction.Output.ForExisting
       (
         -- * Information about an 'AttributeType'

         AttributeTypeInfo(..),
         ATTRIBUTE_OUTPUT_FOR_EXISTING(..),

         -- * Outputers

         outputerObj,
         -- outputerAttrs,

         -- * Configuration of the input form

         AttributeTypeConfiguration(..),

         -- ** The role of an 'AttributeType' in an input form.

         AttributeTypeRole(..),

         -- * Misc

         MappingResult,
         ObjectWithNameFunction,


         -- * Utilities for implementing services

         -- ** Types for all necessary information for implementing these services
         AttributeInfo(..),

         -- * Re-exporting
         
         module Wilde.ObjectModel.UserInteraction.Output.ExistingCommon,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Utils.Utils
import qualified Wilde.Utils.AnyValue as AnyValue

import Wilde.Media.WildeMedia hiding (otKey)
import Wilde.Media.UserInteraction
import Wilde.Media.UserInteraction.Output

import Wilde.ObjectModel.ObjectModelUtils as OmUtils
import qualified Wilde.ObjectModel.AttributeTypeListSetup.WithAnnotation as ListSetupWithAnnotation

import           Wilde.ObjectModel.UserInteraction.Output.ExistingCommon


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Outputer constructors -
-------------------------------------------------------------------------------


type MappingResult a = Either ObjectAndObjectTypeMismatchError a

-- | A function that produces 'LabelAndWidget' for the native value of an 'Attribute'.
data MkAttributeOutput typeFromAttributeType =
  Typeable typeFromAttributeType =>
  MkAttributeOutput (typeFromAttributeType -> ObjectName -> FormBlockRow)

type ObjectWithNameFunction otConf atConf dbTable otNative idAtExisting idAtCreate res =
  (ObjectName, Object otConf atConf dbTable otNative idAtExisting idAtCreate)
  -> MappingResult res


-------------------------------------------------------------------------------
-- - Outputer constructors -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Gives a function that generates a 'FormBlock' for 'Object's for
-- a given 'ObjectType'.
-------------------------------------------------------------------------------
outputerObj :: ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
            => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
            -> AttributeTypeConfigurations atConf dbTable
            -- ^ Specifies the order in which
            -- the 'AttributeType's inputers are listed.
            -- The list must be a permutation of all 'AttributeType's of the
            -- 'ObjectType'.
            -> UserInteractionOutputMonad
               (ObjectWithNameFunction otConf atConf dbTable otNative idAtExisting idAtCreate
                FormBlock)
outputerObj ot atConfigurations =
  do
    setupForRole <- getSetupForRole ot atConfigurations
    setupForMkAttrOutput <- ListSetupWithAnnotation.mapM
                            getMkAttributeOutputForAt
                            setupForRole
    return $ attrOutputsForSetup setupForMkAttrOutput
  where
    getSetupForMkAttrOutput =
      do
        setupForRole <- getSetupForRole ot atConfigurations
        return $
          ListSetupWithAnnotation.mapM
          getMkAttributeOutputForAt
          setupForRole

    getSetupForRole :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                    -> AttributeTypeConfigurations atConf dbTable
                    -> UserInteractionOutputMonad
                       (ListSetupWithAnnotation.Setup
                        otConf atConf dbTable otNative idAtExisting idAtCreate AttributeTypeRole)
    getSetupForRole ot atConfigs = toUserInteractionOutputMonad $
                                   ListSetupWithAnnotation.mkGeneral
                                   ot
                                   atAndRoleList
      where
        atAndRoleList = map
                        (\(AttributeTypeConfiguration role at) -> (at,role))
                        atConfigs

    getMkAttributeOutputForAt :: ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
                              => (Any (AttributeType atConf dbTable),AttributeTypeRole)
                              -> UserInteractionOutputMonad
                                 (AnyValue.Container MkAttributeOutput)
    getMkAttributeOutputForAt (Any at,role) =
      getMkAttributeOutput
      (role,
       AnyValue.Container (at2ati at))


-------------------------------------------------------------------------------
-- - Utilities -
-------------------------------------------------------------------------------


attrOutputsForSetup :: ListSetupWithAnnotation.Setup 
                       otConf atConf dbTable otNative idAtExisting idAtCreate
                       (AnyValue.Container MkAttributeOutput)
                    -> (ObjectName, Object otConf atConf dbTable otNative idAtExisting idAtCreate)
                    -> MappingResult FormBlock
attrOutputsForSetup setup (objectName,o) =
  do
    formBlockRows <- mapM (attrOutputAny objectName) mkaoAndattriList
    return $ FormBlock formBlockRows []
  where
    mkaoAndattriList = map
                       (\(Any attr,anyMkAo) ->
                         (anyMkAo,
                          AnyValue.Container (attr2attri attr)))
                       attrAndMkAoList
    attrAndMkAoList = ListSetupWithAnnotation.apply setup o

attrOutputAny :: ObjectName
              -> (AnyValue.Container MkAttributeOutput,
                  AnyValue.Container AttributeInfo)
              -> MappingResult FormBlockRow
attrOutputAny objectName
  (AnyValue.Container mkAttributeOutput,
   AnyValue.Container ai)
  =
  attrOutput undefined objectName ai mkAttributeOutput

attrOutput :: fromForExistingFromAttributeType
              -- ^ Needed just for typing/errmsg.
              -- Never accessed (so undefined is a fine value).
           -> ObjectName
           -> AttributeInfo typeForExisting
           -> MkAttributeOutput fromForExistingFromAttributeType
           -> MappingResult FormBlockRow
attrOutput undefinedForErrMsg objectName
  ai@(AttributeInfo {aiCrossRefKey = theCrossRefKey,aiValue = theValue})
  (MkAttributeOutput mkAttributeOutput) =
  case cast theValue of
    Nothing ->
      let
        errMsg   = "type error for attribute with key: " ++ theCrossRefKey
        mismatch = Mismatch (typeOf theValue) (typeOf undefinedForErrMsg)
      in
       Left $ ObjectAndObjectTypeMismatchError errMsg (AttributeTypeError mismatch)
    Just castedValue -> return $ mkAttributeOutput castedValue objectName

getMkAttributeOutput :: (AttributeTypeRole,AnyValue.Container AttributeTypeInfo)
                     -> UserInteractionOutputMonad (AnyValue.Container MkAttributeOutput)
getMkAttributeOutput (role,AnyValue.Container ati@(AttributeTypeInfo {})) =
   do
     mkForMaybe <- getMkAttributeOutputFun (role,ati)
     return $ AnyValue.Container $ MkAttributeOutput (mkForMaybe . Just)

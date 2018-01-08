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

-------------------------------------------------------------------------------
-- | Types related to output of 'AttributeType's.
-------------------------------------------------------------------------------
module Wilde.ObjectModel.UserInteraction.OutputTypes
       (
         -- * Fixed values
         
         AttributeFixedValues,
         AttributeFixedValue(..),
         lookupFixedValue,
         
         -- * Outputer for create
         
         AttributeWidgetDefaultValueForCreate(..),
         AttributeTypeOutputerForCreate(..),
         
         -- * Ouputer for existing
         
         AttributeUiDefaultForExisting,
         UserInteractionOutputerForExisting,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.List

import Wilde.Media.UserInteraction
import qualified Wilde.Media.GenericStringRep as Gsr
import qualified Wilde.Media.UserInteraction.Output as UiO

import Wilde.ObjectModel.ObjectModel


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Fixed Values -
-------------------------------------------------------------------------------


-- | Fixed values for some or all of the 'AttributeType's of an 'ObjectType'.
type AttributeFixedValues atConf dbTable = [AttributeFixedValue atConf dbTable]

-- | A fixed value for an 'AttributeType' - the user cannot supply a
-- different value.
data AttributeFixedValue atConf dbTable =
  AttributeFixedValue
  {
    fixedType  :: Any (AttributeType atConf dbTable)
  , fixedValue :: Gsr.GenericStringRep
    -- ^ This value is fixed in the Form Block.  The inputer should
    -- detect and input it.
  }

-- | Looks up a default value for a given 'AttributeType'.
lookupFixedValue :: AttributeFixedValues atConf dbTable
                 -> AttributeName
                 -> Maybe Gsr.GenericStringRep
lookupFixedValue defaults atTargetKey =
  maybe Nothing (Just . fixedValue) $ find isMatch defaults
  where
    isMatch :: AttributeFixedValue atConf dbTable -> Bool
    isMatch (AttributeFixedValue at _) =
      atTargetKey == atCrossRefKey_anyValue at


-------------------------------------------------------------------------------
-- - Create AttributeWidgetDefaultValueForCreate -
-------------------------------------------------------------------------------


-- | A 'WidgetConstructorGetter' of a create-value.
--
-- Fixes the type of default value.
type AttributeTypeOutputerForCreate typeForExisting typeForCreate =
  UiO.WidgetConstructorGetter
  (AttributeWidgetDefaultValueForCreate typeForExisting typeForCreate)

-- | Default value for an 'Attribute' when outputing an UI for creating
-- an 'Object'.
data AttributeWidgetDefaultValueForCreate typeForExisting typeForCreate
  = DefaultCreateFromUiPreFill GenericWidgetDefaultValue
  | DefaultCreateFromExisting typeForExisting
  | DefaultCreateFromCreate   typeForCreate


-------------------------------------------------------------------------------
-- - Existing -
-------------------------------------------------------------------------------


type UserInteractionOutputerForExisting a = AttributeName ->
                                            UiO.WidgetConstructorGetter
                                            (AttributeUiDefaultForExisting a)

-- | Default value when outputing UI for for an existing 'Object'.
type AttributeUiDefaultForExisting typeForExisting = typeForExisting

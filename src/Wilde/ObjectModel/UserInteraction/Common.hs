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

-- | Functionallity that is common to output for create.
module Wilde.ObjectModel.UserInteraction.Common
       (
         metaValuesForRole,
         
         inputValueForRoleFromEnv,
         Role(..),
         
         inputFixedFromEnv,
         inputDefaultFromEnv,
         
         elementKeyForRoleIndicator,
         elementKeyForAttributeValue,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------

import Data.Char

import qualified Wilde.Media.ElementSet as ES
import Wilde.Media.MonadWithInputMedia (MonadWithInputMedia(..),inInputMedia_raw)
import qualified Wilde.Media.UserInteraction.Output as UiO
import qualified Wilde.Media.GenericStringRep as Gsr

import Wilde.ObjectModel.ObjectModel


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | The meta values that stores a value for a role.
-------------------------------------------------------------------------------
metaValuesForRole :: Role
                  -> AttributeName 
                  -> UiO.ObjectName 
                  -> String
                  -> [ES.Element]
metaValuesForRole role attributeName objectName value = 
  [
    ES.element keyForValue     value
  , ES.element keyForIndicator (indicatorForRole role)
  ]
  where
    keyForValue     = UiO.elementKey objectName attributeName
    keyForIndicator = elementKeyForRoleIndicator attributeName objectName

-------------------------------------------------------------------------------
-- | The Element Key for the element that holds the indicator of which
-- role the attribute's value has.
-------------------------------------------------------------------------------
elementKeyForRoleIndicator :: AttributeName
                           -> UiO.ObjectName
                           -> ES.ElementKey
elementKeyForRoleIndicator attributeName objectName =
  ES.elementKey
  (ES.elementKeyPrefixAdd objectName attributeName)
  roleIndicator

elementKeyForAttributeValue :: AttributeName
                            -> UiO.ObjectName
                            -> ES.ElementKey
elementKeyForAttributeValue attributeName objectName = 
  ES.elementKey objectName attributeName

-- | The element that indicates the role of the attribute's value.
roleIndicator :: AttributeName
roleIndicator = "_role"

-- | Role of attribut value in env.
data Role = Fix
          | Default
          deriving (Eq,Enum,Show,Read)

indicatorForRole :: Role -> String
indicatorForRole = show

indicatesRole :: String -> Role -> Bool
indicatesRole indicator role = 
  toLowerString indicator == toLowerString (show role)
  where
    toLowerString = map toLower

-------------------------------------------------------------------------------
-- | Gets the String for a given Role for an attribute.
--
-- Gives the error 'ValueMissing' if the attribute does not have
-- a value.
-------------------------------------------------------------------------------
lookupStringForRole :: Role 
                    -> AttributeName 
                    -> UiO.ObjectName
                    -> ES.Lookuper (Maybe String)
lookupStringForRole requestedRole attributeName objectName es =
  case ES.lookupSingleton_optional keyForRole es of
    Left err        -> return Nothing
    Right Nothing   -> return Nothing
    Right (Just indicator) -> 
      if indicator `indicatesRole` requestedRole
      then
        case ES.lookupSingleton_optional keyForValue es of
          Right mbV -> return $ Just $ maybe Gsr.emptyValue id mbV
          Left err  -> Left err
      else
        return Nothing
  where
    keyForValue = elementKeyForAttributeValue attributeName objectName
    keyForRole  = elementKeyForRoleIndicator  attributeName objectName

-------------------------------------------------------------------------------
-- | Gets the fixed Generic String Representation for an attribute.
--
-- Return value
--
-- [@Nothing@] There is no fixed value in environment.
-- [@Just Nothing@] There is a fixed value, but the actual value is
--                  empty=missing. 
-- Gives the error 'ValueMissing' if the attribute does not have
-- fixed value.
-------------------------------------------------------------------------------
inputFixedFromEnv :: MonadWithInputMedia m
                  => AttributeName
                  -> UiO.ObjectName
                  -> m (Maybe Gsr.GenericStringRep)
inputFixedFromEnv = inputValueForRoleFromEnv Fix

-------------------------------------------------------------------------------
-- | Gets the Widget Default Value for an attribute.
--
-- See 'inputFixedFromEnv' for info about return value.
-------------------------------------------------------------------------------
inputDefaultFromEnv :: MonadWithInputMedia m
                    => AttributeName
                    -> UiO.ObjectName
                    -> m (Maybe Gsr.GenericStringRep)
inputDefaultFromEnv = inputValueForRoleFromEnv Default

-------------------------------------------------------------------------------
-- | Gets the Value for a Role, for an attribute.
--
-- Return value
--
-- [@Nothing@] There is no fixed value in environment.
-- [@Just Nothing@] There is a fixed value, but the actual value is
--                  empty=missing. 
-- Gives the error 'ValueMissing' if the attribute does not have
-- fixed value.
-------------------------------------------------------------------------------
inputValueForRoleFromEnv :: MonadWithInputMedia m
                         => Role
                         -> AttributeName
                         -> UiO.ObjectName
                         -> m (Maybe String)
inputValueForRoleFromEnv requestedRole attributeName objectName =
  do
    res <- inInputMedia_raw
           (lookupStringForRole requestedRole attributeName objectName)
    case res of
      Right x -> 
        return x
      Left (_, ES.ValueMissing,_) -> 
        return Nothing
      _ -> 
        error "inputValueForRoleFromEnv: Implementation Error"

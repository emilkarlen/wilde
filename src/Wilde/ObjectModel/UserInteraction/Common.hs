-- | Functionallity that is common to output for create.
module Wilde.ObjectModel.UserInteraction.Common
(
  metaValuesForRole,

  inputValueForRoleFromEnv,
  getLookuperOfInputValueForRoleFromEnv,

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


import           Data.Char

import qualified Wilde.Media.ElementSet as ES
import           Wilde.Media.MonadWithInputMedia (MonadWithInputMedia(..))
import qualified Wilde.Media.UserInteraction.Output as UiO
import qualified Wilde.Media.GenericStringRep as Gsr

import           Wilde.ObjectModel.ObjectModel


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
    Left err        -> pure Nothing
    Right Nothing   -> pure Nothing
    Right (Just indicator) ->
      if indicator `indicatesRole` requestedRole
      then
        case ES.lookupSingleton_optional keyForValue es of
          Right mbV -> pure $ Just $ maybe Gsr.emptyValue id mbV
          Left err  -> Left err
      else
        pure Nothing
  where
    keyForValue = elementKeyForAttributeValue attributeName objectName
    keyForRole  = elementKeyForRoleIndicator  attributeName objectName

-------------------------------------------------------------------------------
-- | Gets the fixed Generic String Representation for an attribute.
--
-- pure value
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
-- See 'inputFixedFromEnv' for info about pure value.
-------------------------------------------------------------------------------
inputDefaultFromEnv :: MonadWithInputMedia m
                    => AttributeName
                    -> UiO.ObjectName
                    -> m (Maybe Gsr.GenericStringRep)
inputDefaultFromEnv = inputValueForRoleFromEnv Default

-------------------------------------------------------------------------------
-- | Gets the Value for a Role, for an attribute.
--
-- pure value
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
    inputMedia <- getInputMedia
    pure $ lookuperOfInputValueForRoleFromEnv inputMedia requestedRole attributeName objectName

getLookuperOfInputValueForRoleFromEnv
  :: MonadWithInputMedia m
  => m (Role -> AttributeName -> UiO.ObjectName -> Maybe String)
getLookuperOfInputValueForRoleFromEnv =
    lookuperOfInputValueForRoleFromEnv <$> getInputMedia


-------------------------------------------------------------------------------
-- | Gets the Value for a Role, for an attribute.
--
-- [@Nothing@] There is no fixed value in environment.
-- [@Just Nothing@] There is a fixed value, but the actual value is
--                  empty=missing.
-- Gives the error 'ValueMissing' if the attribute does not have
-- fixed value.
-------------------------------------------------------------------------------
lookuperOfInputValueForRoleFromEnv
  :: ES.ElementSet
  -- ^ input media for the service
  -> Role
  -> AttributeName
  -> UiO.ObjectName
  -> Maybe String
lookuperOfInputValueForRoleFromEnv inputMedia requestedRole attributeName objectName =
  do
    let res = lookupStringForRole requestedRole attributeName objectName inputMedia
    case res of
      Right x ->
        x
      Left (_, ES.ValueMissing,_) ->
        Nothing
      Left x ->
        error $ "inputValueForRoleFromEnv: Implementation Error: " <> show x

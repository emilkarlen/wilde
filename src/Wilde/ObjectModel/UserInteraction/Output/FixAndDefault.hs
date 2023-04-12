{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.ObjectModel.UserInteraction.Output.FixAndDefault
(
  AttributeFixAndDefaultResolver,
  FixValuePrecedence(..),
  FixAndDefault(..),
  FixAndDefaultForApplicationConfiguration(..),
  FixAndDefaultForEnvironment(..),
  FixFromEnv,

  resolveFixValue,
  resolveWidgetDefaultValue,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Media.UserInteraction.Output as UiO
import qualified Wilde.Media.GenericStringRep as Gsr

import           Wilde.ObjectModel.ObjectModel


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - handling of fix and default values -
-------------------------------------------------------------------------------


data FixAndDefault d a =
  FixAndDefault
  {
    appFad :: FixAndDefaultForApplicationConfiguration d
  , envFad :: FixAndDefaultForEnvironment d a
  }

-------------------------------------------------------------------------------
-- | Fix and default value from the Application Configuration.
--
-- An actual fix value is not present since it should be derived from the
-- appliation source code when inputing the form.  Thus it is never
-- used for output.
--
-- A default value, on the other hand, is used in widget output, so
-- it must be present here.
-------------------------------------------------------------------------------
data FixAndDefaultForApplicationConfiguration d =
  FixAndDefaultForApplicationConfiguration
  {
    appValsFix     :: Maybe FixValuePrecedence
  , appValsDefault :: Maybe d
  }

-- | Tells which value has precedence if several fix values exist.
data FixValuePrecedence = FixFromApplicationHasPrecedence
                        | FixFromEnvironmentHasPrecedence
                          deriving (Show,Enum)

data FixAndDefaultForEnvironment d a =
  FixAndDefaultForEnvironment
  {
    envValsFix     :: Maybe (FixFromEnv a)
  , envValsDefault :: Maybe d
  }

type FixFromEnv a = Either Gsr.GenericStringRep a

mkEnvFix_gsr :: Gsr.GenericStringRep -> FixFromEnv a
mkEnvFix_gsr = Left

mkEnvFix_value :: a -> FixFromEnv a
mkEnvFix_value = Right


-- | Constructor of a 'FixAndDefaultResolver' for an attribute.
--
-- The attribute has a name and is part of an object.
-- This information is needed for resoving default values from
-- the "input media".
type AttributeFixAndDefaultResolver d a =
  UiO.Monad (AttributeName -> UiO.ObjectName -> FixAndDefault d a)

-------------------------------------------------------------------------------
-- | Gives the default value for the widget, or no value, if there
-- is no such default.
--
-- Helper for 'resolveAttributeOutputValueSpecification'.
-------------------------------------------------------------------------------
resolveWidgetDefaultValue :: Maybe d
                          -- ^ Resolver corresponding to the Application Configuration
                          -> Maybe d
                          -- ^ Resolver for the User Interactino Environment.
                          -> Maybe d
resolveWidgetDefaultValue mbDefaultFromAppConfig mbDefaultFromEnv =
  case mbDefaultFromEnv of
    Nothing -> mbDefaultFromAppConfig
    justX   -> justX

-------------------------------------------------------------------------------
-- | Gives the fixed value to output in the form, if one should
-- be output.
--
-- [@Nothing@] No fixed value.
--
-- [@Just Nothing@] There is a fixed value, but nothing should
-- be output in the form, since the value is hard coded in the
-- application configuration.
--
-- [@Just (Just x)@] There is a fixed value that
-- should be output in the form.
--
-- Helper for 'resolveAttributeOutputValueSpecification'.
-------------------------------------------------------------------------------
resolveFixValue :: Maybe FixValuePrecedence
                -- ^ config from application
                -> Maybe a
                -- ^ config from environment
                -> Maybe (Maybe a)
resolveFixValue Nothing fixValueFromEnv = fmap Just fixValueFromEnv

resolveFixValue (Just FixFromApplicationHasPrecedence) fixValueFromEnv =
  Just Nothing

resolveFixValue (Just FixFromEnvironmentHasPrecedence) fixValueFromEnv =
  maybe
    (Just Nothing)
    (Just . Just)
    fixValueFromEnv

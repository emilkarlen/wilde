{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.ObjectModel.UserInteraction.Output.FixAndDefault
       (
         -- * Handling of fix and default values

         AttributeTypeInfo(..),

         -- ** Resolving of fix and default value

         AttributeOutputValueSpecification(..),

         FixValuePrecedence(..),

         AttributeFixAndDefaultResolver,
         FixAndDefault(..),
         FixAndDefaultForApplicationConfiguration(..),
         FixAndDefaultForEnvironment(..),

         FixFromEnv,
         mkEnvFix_gsr,
         mkEnvFix_value,

         -- * Construction of a FormBlock

         AttributeTypeSetup(..),
         attributeOutputer,
         outputerForSetupConstructor,
         objectOutputer,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Utils.AnyValue2 as AnyValue2

import qualified Wilde.Media.UserInteraction.Output as UiO
import qualified Wilde.Media.GenericStringRep as Gsr

import           Wilde.ObjectModel.ObjectModel

import qualified Wilde.ObjectModel.UserInteraction.Common as UiCommon

import           Wilde.WildeUi.UiPrimitives


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Fixed Value in Environment -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Information about an 'AttributeType' that makes it possible to construct
-- an UI outputer for it.
--
-- The outputer supports a default value of a parametrized type
-- @defaultValue@ and possibly an existing value of type @attrValue@.
-------------------------------------------------------------------------------
data AttributeTypeInfo defaultValue attrValue =
  AttributeTypeInfo
  {
    atiCrossRefKey    :: AttributeName
  , atiTitle          :: WildeTitle
  , atiWidgetOutputer :: AttributeName -> UiO.WidgetConstructorGetter defaultValue
  , atiGsrOutputer    :: Gsr.GenericStringRepOutputer attrValue
  }


-------------------------------------------------------------------------------
-- - handling of fix and default values -
-------------------------------------------------------------------------------


-- | Specifies what is output for an attribute in an input form.
data AttributeOutputValueSpecification d a
  = NoOutput
    -- ^ The attribute is abscent from the form.
  | FixValue (Either Gsr.GenericStringRep a)
    -- ^ The attribute has a fixed value so the form should contain no widget
    -- for inputing a value.
    --
    -- The fix value may be displayed in the form or not.
  | WidgetWithPossibleDefaultValue (Maybe d)
    -- ^ Either 'Just' a default value, or no default value.
    --
    -- The form should contain an input widget for the attribute.
    -- If there is Just a value, then the widget should have this
    -- value preselected.
    -- Otherwise the widget decides itself what value should be
    -- preselected.

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
  UiO.Monad ( AttributeName -> UiO.ObjectName -> FixAndDefault d a)


-------------------------------------------------------------------------------
-- | Resolves the \"Attribute Output Value Specification\" using the
-- standard preferences, given resovlers for fix and default values.
-------------------------------------------------------------------------------
resolveAttributeOutputValueSpecification
  :: forall d a.
     (UiO.ObjectName -> FixAndDefault d a)
  -> UiO.ObjectName
  -> AttributeOutputValueSpecification d a
resolveAttributeOutputValueSpecification on2Fad objectName =
  resolve (on2Fad objectName)
  where
    resolve :: FixAndDefault d a -> AttributeOutputValueSpecification d a
    resolve
      (FixAndDefault
       appConf@(FixAndDefaultForApplicationConfiguration appFix appDefault)
       envConf@(FixAndDefaultForEnvironment envFix envDefault))
      =
        maybe
          outputIsEitherDefaultValueOrNoValue
          outputIsFixValue
          mbFixValue
      where
        mbFixValue = resolveFixValueP appFix envFix

        outputIsFixValue Nothing  = NoOutput
        outputIsFixValue (Just x) = FixValue x

        outputIsEitherDefaultValueOrNoValue =
            let
              mbDefaultValue = resolveWidgetDefaultValueP appDefault envDefault
            in
              WidgetWithPossibleDefaultValue mbDefaultValue

-------------------------------------------------------------------------------
-- | Gives the default value for the widget, or no value, if there
-- is no such default.
--
-- Helper for 'resolveAttributeOutputValueSpecification'.
-------------------------------------------------------------------------------
resolveWidgetDefaultValue :: UiO.Monad (Maybe d)
                          -- ^ Resolver corresponding to the Application Configuration
                          -> UiO.Monad (Maybe d)
                          -- ^ Resolver for the User Interactino Environment.
                          -> UiO.Monad (Maybe d)
resolveWidgetDefaultValue resolverForAppConfig resolverForEnvironment =
  do
    mbDefaultFromEnv <- resolverForEnvironment
    case mbDefaultFromEnv of
      Nothing -> resolverForAppConfig
      justX   -> pure justX

resolveWidgetDefaultValueP :: Maybe d
                          -- ^ Resolver corresponding to the Application Configuration
                          -> Maybe d
                          -- ^ Resolver for the User Interactino Environment.
                          -> Maybe d
resolveWidgetDefaultValueP mbDefaultFromAppConfig mbDefaultFromEnv =
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
resolveFixValueP :: Maybe FixValuePrecedence
                -- ^ config from application
                -> Maybe a
                -- ^ config from environment
                -> Maybe (Maybe a)
resolveFixValueP Nothing fixValueFromEnv = fmap Just fixValueFromEnv

resolveFixValueP (Just FixFromApplicationHasPrecedence) fixValueFromEnv =
  Just Nothing

resolveFixValueP (Just FixFromEnvironmentHasPrecedence) fixValueFromEnv =
  maybe
    (Just Nothing)
    (Just . Just)
    fixValueFromEnv


-------------------------------------------------------------------------------
-- - Construction of UI ouput for an Attribute Type -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | The information about an \"Attribute Type\" necessary to produce
-- User Interaction Output for a single attribute.
-------------------------------------------------------------------------------
data AttributeTypeSetup d a =
  AttributeTypeSetup
  {
    setupInfo     :: AttributeTypeInfo d a
  , setupResolver :: AttributeFixAndDefaultResolver d a
  }

attributeOutputer :: forall d a.
                     AttributeTypeSetup d a
                  -> UiO.Monad (UiO.ObjectName -> UiO.FormBlockRowInfo)
attributeOutputer (AttributeTypeSetup atInfo getFadCon) =
  do
    fadCon            <- getFadCon
    widgetConstructor <- atiWidgetOutputer atInfo attributeName
    let
      on2fad  :: UiO.ObjectName -> FixAndDefault d a
      on2fad   = fadCon attributeName

      on2ovs  :: UiO.ObjectName -> AttributeOutputValueSpecification d a
      on2ovs   = resolveAttributeOutputValueSpecification on2fad

    pure $ retVal on2ovs widgetConstructor
  where
    attributeName :: AttributeName
    attributeName  = atiCrossRefKey atInfo

    theGsrOutputer = atiGsrOutputer atInfo

    retVal :: (UiO.ObjectName -> AttributeOutputValueSpecification d a)
           -> UiO.WidgetConstructorForObjectWithDefault d
           -> UiO.ObjectName
           -> UiO.FormBlockRowInfo
    retVal on2ovs widgetCon objectName = retVal' (on2ovs objectName)
      where
        retVal' :: AttributeOutputValueSpecification d a
                -> UiO.FormBlockRowInfo
        retVal' NoOutput = empty

        retVal' (FixValue fixValue) =
          UiO.mkFormBlockRowInfoForMetas $
          UiCommon.metaValuesForRole UiCommon.Fix attributeName objectName $
          either id theGsrOutputer fixValue

        retVal' (WidgetWithPossibleDefaultValue mbDefaultValue) =
          UiO.mkFormBlockRowInfoForLabelAndWidget (label,widget)
          where
            widget      = widgetCon mbDefaultValue objectName
            label       = UiO.Label ek title
            title       = wildeStyled $ atiTitle atInfo
            ek          = UiO.elementKey objectName attributeName


-------------------------------------------------------------------------------
-- | Constructs an outputer for 'AttributeType's for a single 'ObjectType'.
-------------------------------------------------------------------------------
outputerForSetupConstructor :: (Any (AttributeType atConf dbTable)
                                -> AnyValue2.Container AttributeTypeSetup)
                            -- ^ Constructs setup for an 'AttributeType'.
                            -> [Any (AttributeType atConf dbTable)]
                            -- ^ The attributes that should be input via the form,
                            -- and the order they should be displayed in it.
                            -> UiO.Monad (UiO.ObjectName -> UiO.FormBlock)
outputerForSetupConstructor setupConstructor attributeTypesOrder =
  objectOutputer atSetups_any
  where
    atSetups_any = map setupConstructor attributeTypesOrder

-------------------------------------------------------------------------------
-- | Outputs a list of attributes as part of an object.
-------------------------------------------------------------------------------
objectOutputer :: [AnyValue2.Container AttributeTypeSetup]
               -> UiO.Monad (UiO.ObjectName -> UiO.FormBlock)
objectOutputer setups =
  do
    attrOutputers <- mapM mkAttrOutputter setups
    pure $ retVal attrOutputers
  where
    retVal :: [UiO.ObjectName -> UiO.FormBlockRowInfo]
           -> UiO.ObjectName -> UiO.FormBlock
    retVal attrOutputers objectName =
      UiO.concatAtFormBlockInfos
        [attrOutputer objectName | attrOutputer <- attrOutputers]

    mkAttrOutputter :: AnyValue2.Container AttributeTypeSetup
                    -> UiO.Monad (UiO.ObjectName -> UiO.FormBlockRowInfo)
    mkAttrOutputter (AnyValue2.Container atSetup) = attributeOutputer atSetup

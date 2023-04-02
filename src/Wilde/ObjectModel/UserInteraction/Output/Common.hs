{-# LANGUAGE ExistentialQuantification #-}

module Wilde.ObjectModel.UserInteraction.Output.Common
       (
         -- * Handling of fix and default values

         AttributeTypeInfo(..),

         -- ** Resolving of fix and default value

         AttributeOutputValueSpecification(..),

         FixValuePrecedence(..),

         FixAndDefaultResolver(..),
         FixAndDefaultResolverForEnvironment(..),
         FixAndDefaultResolverForApplicationConfiguration(..),

         AttributeFixAndDefaultResolverConstructor,
         attributeOutputForValue,
         resolveAttributeOutputValueSpecification,
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

import qualified Wilde.Media.UserInteraction as UI
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

data FixAndDefaultResolver d a =
  FixAndDefaultResolver
  {
    appResolver :: FixAndDefaultResolverForApplicationConfiguration d
  , envResolver :: FixAndDefaultResolverForEnvironment d a
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
data FixAndDefaultResolverForApplicationConfiguration d =
  FixAndDefaultResolverForApplicationConfiguration
  {
    appFix     :: Maybe FixValuePrecedence
  , appDefault :: UiO.UserInteractionOutputMonad (Maybe d)
  }

-- | Tells which value has precedence if several fix values exist.
data FixValuePrecedence = FixFromApplicationHasPrecedence
                        | FixFromEnvironmentHasPrecedence
                          deriving (Show,Enum)

data FixAndDefaultResolverForEnvironment d a =
  FixAndDefaultResolverForEnvironment
  {
    envFix     :: UiO.UserInteractionOutputMonad (Maybe (FixFromEnv a))
  , envDefault :: UiO.UserInteractionOutputMonad (Maybe d)
  }

type FixFromEnv a = Either Gsr.GenericStringRep a

mkEnvFix_gsr :: Gsr.GenericStringRep -> FixFromEnv a
mkEnvFix_gsr = Left

mkEnvFix_value :: a -> FixFromEnv a
mkEnvFix_value = Right


-- | Constructor of a 'FixAndDefaultResolver' for an attribute.
--
-- The attribute has a name and is part of an object.
type AttributeFixAndDefaultResolverConstructor d a =
  AttributeName ->
  UiO.ObjectName ->
  FixAndDefaultResolver d a

-------------------------------------------------------------------------------
-- | Constructs the form output for a single attribute, given setup
-- and \"Output Value Specification\".
-------------------------------------------------------------------------------
attributeOutputForValue :: AttributeTypeInfo d a
                        -> AttributeOutputValueSpecification d a
                        -> UiO.ObjectName
                        -> UiO.UserInteractionOutputMonad UiO.FormBlockRowInfo
attributeOutputForValue _ NoOutput _ = pure empty
attributeOutputForValue (AttributeTypeInfo {
                            atiGsrOutputer = theGsrOutputer,
                            atiCrossRefKey = theAttributeName
                            })
  (FixValue fixValue)
  objectName
  =
  pure $
  UiO.mkFormBlockRowInfoForMetas $
  UiCommon.metaValuesForRole UiCommon.Fix theAttributeName objectName $
  either id theGsrOutputer fixValue

attributeOutputForValue (AttributeTypeInfo {
                            atiCrossRefKey    = theAttributeName,
                            atiTitle          = theTitle,
                            atiGsrOutputer    = theGsrOutputer,
                            atiWidgetOutputer = theWidgetOutputerGetter
                            })
  (WidgetWithPossibleDefaultValue mbDefaultValue)
  objectName
  =
  do
    widgetConstructor <- theWidgetOutputerGetter theAttributeName
    let widget = widgetConstructor mbDefaultValue objectName
    pure $
      UiO.mkFormBlockRowInfoForLabelAndWidget (label,widget)
  where
    ek          = UiO.elementKey objectName theAttributeName
    titleString = wildeStyled theTitle
    label       = UiO.Label ek titleString

-------------------------------------------------------------------------------
-- | Resolves the \"Attribute Output Value Specification\" using the
-- standard preferences, given resovlers for fix and default values.
-------------------------------------------------------------------------------
resolveAttributeOutputValueSpecification :: FixAndDefaultResolver d a
                                         -> UiO.UserInteractionOutputMonad
                                            (AttributeOutputValueSpecification d a)
resolveAttributeOutputValueSpecification (FixAndDefaultResolver
                             appConf@(FixAndDefaultResolverForApplicationConfiguration appFix appDefault)
                             envConf@(FixAndDefaultResolverForEnvironment envFix envDefault))
  =
  do
    mbFixValue <- resolveFixValue appFix envFix
    maybe
      outputIsEitherDefaultValueOrNoValue
      outputIsFixValue
      mbFixValue
  where
    outputIsFixValue Nothing  = pure NoOutput
    outputIsFixValue (Just x) = pure (FixValue x)

    outputIsEitherDefaultValueOrNoValue =
      do
        mbDefaultValue <- resolveWidgetDefaultValue appDefault envDefault
        pure $ WidgetWithPossibleDefaultValue mbDefaultValue

-------------------------------------------------------------------------------
-- | Gives the default value for the widget, or no value, if there
-- is no such default.
--
-- Helper for 'resolveAttributeOutputValueSpecification'.
-------------------------------------------------------------------------------
resolveWidgetDefaultValue :: UiO.UserInteractionOutputMonad (Maybe d)
                          -- ^ Resolver corresponding to the Application Configuration
                          -> UiO.UserInteractionOutputMonad (Maybe d)
                          -- ^ Resolver for the User Interactino Environment.
                          -> UiO.UserInteractionOutputMonad (Maybe d)
resolveWidgetDefaultValue resolverForAppConfig resolverForEnvironment =
  do
    mbDefaultFromEnv <- resolverForEnvironment
    case mbDefaultFromEnv of
      Nothing -> resolverForAppConfig
      justX   -> pure justX

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
                -> UiO.UserInteractionOutputMonad (Maybe a)
                -- ^ config from environment
                -> UiO.UserInteractionOutputMonad (Maybe (Maybe a))
resolveFixValue Nothing getEnvFix =
  do
    fixValueFromEnv <- getEnvFix
    pure $ maybe
      Nothing
      (Just . Just)
      fixValueFromEnv

resolveFixValue (Just FixFromApplicationHasPrecedence) getEnvFix =
  pure (Just Nothing)

resolveFixValue (Just FixFromEnvironmentHasPrecedence) getEnvFix =
  do
    fixValueFromEnv <- getEnvFix
    pure $ maybe
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
    setupInfo                :: AttributeTypeInfo d a
  , setupResolverConstructor :: AttributeFixAndDefaultResolverConstructor d a
  }

attributeOutputer :: AttributeTypeSetup d a
                  -> UiO.ObjectName
                  -> UiO.UserInteractionOutputMonad UiO.FormBlockRowInfo
attributeOutputer (AttributeTypeSetup atInfo resolverCon) objectName =
  do
    let resolver       = resolverCon attributeName objectName
    attrOutputValSpec <- resolveAttributeOutputValueSpecification resolver
    attributeOutputForValue atInfo attrOutputValSpec objectName
  where
    attributeName = atiCrossRefKey atInfo

-------------------------------------------------------------------------------
-- | Constructs an outputer for 'AttributeType's for a single 'ObjectType'.
-------------------------------------------------------------------------------
outputerForSetupConstructor :: (Any (AttributeType atConf dbTable)
                                -> AnyValue2.Container AttributeTypeSetup)
                            -- ^ Constructs setup for an 'AttributeType'.
                            -> [Any (AttributeType atConf dbTable)]
                            -- ^ The attributes that should be input via the form,
                            -- and the order they should be displayed in it.
                            -> WildeStyle
                            -- ^ Style of the FormBlock
                            -> UiO.ObjectName
                            -> UiO.UserInteractionOutputMonad UiO.FormBlock
outputerForSetupConstructor setupConstructor attributeTypesOrder =
  objectOutputer atSetups_any
  where
    atSetups_any = map setupConstructor attributeTypesOrder

-------------------------------------------------------------------------------
-- | Outputs a list of attributes as part of an object.
-------------------------------------------------------------------------------
objectOutputer :: [AnyValue2.Container AttributeTypeSetup]
               -> WildeStyle
               -- ^ Style of the FormBlock
               -> UiO.ObjectName
               -> UiO.UserInteractionOutputMonad UiO.FormBlock
objectOutputer setups formBlockStyle objectName =
  do
    formBlockRowInfos <- mapM
                         (\(AnyValue2.Container atSetup)
                          -> attributeOutputer atSetup objectName)
                         setups
    pure $ UI.formBlock_appendStyle formBlockStyle $ UiO.concatAtFormBlockInfos formBlockRowInfos

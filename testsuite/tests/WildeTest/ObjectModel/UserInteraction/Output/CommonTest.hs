module WildeTest.ObjectModel.UserInteraction.Output.CommonTest
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import Wilde.Utils.Empty

import Wilde.WildeUi.WildeStyle

import qualified Wilde.Media.ElementSet as ES
import qualified Wilde.ObjectModel.UserInteraction.Output.FixAndDefault as Common
import qualified Wilde.Media.UserInteraction.Output as UiO
import qualified Wilde.Media.UserInteraction as UserInteraction
import qualified Wilde.ObjectModel.UserInteraction.Common as UserInteractionCommon

import qualified TestResources.UserInteractionOutputMonadUtils as TestUiUtils
import qualified TestResources.UserInteraction.WidgetResources as WidgetResources
import qualified TestResources.UserInteraction.FormBlockResources as FormBlockResources
import qualified TestResources.Testing.AssertUtils as AssertUtils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theTest :: Test
theTest =
  "attributeOutputer" ~: TestList
  [
    "handlig of fix values"     ~: handlingOfFixValuesTest
  , "handlig of default values" ~: handlingOfDefaultValuesTest
  ]

handlingOfFixValuesTest :: Test
handlingOfFixValuesTest =
  TestList
  [ "WHEN there is no precedence for fixed values, " ++
    "THEN a plain widget SHOULD be used" ~:

    (resolver_with_no_defaults_but_fixeds_for_app_and_env
     Nothing
     Nothing)
    `shouldGive`
    (formBlockRowInfo_widget widgetWithoutDefault)


  , "WHEN value from Environment has precedence but does not exist, " ++
    "THEN the value from the Application Config SHOULD be used" ~:

    (resolver_for_fixeds_for_app_and_env
     (Just Common.FixFromEnvironmentHasPrecedence)
     Nothing)
    `shouldGive_forAllCombinationsOfDefaultConfig`
    empty

  , "WHEN all types of fixed values are present, " ++
    "THEN precedence SHOULD be according to application config" ~: TestList
    [
      "precedence from app" ~:

      (resolver_for_fixeds_for_app_and_env
       (Just Common.FixFromApplicationHasPrecedence)
       (Just (Common.mkEnvFix_value theFix_env_value)))
      `shouldGive_forAllCombinationsOfDefaultConfig`
      empty

    , "precedence from env (fix from env in the form of \"value\")" ~:

      (resolver_for_fixeds_for_app_and_env
       (Just Common.FixFromEnvironmentHasPrecedence)
       (Just (Common.mkEnvFix_value theFix_env_value)))
      `shouldGive_forAllCombinationsOfDefaultConfig`
      formBlockRowInfo_gsrMeta theFix_env_value

    , "precedence from env (fix from env in the form of \"GSR\")" ~:

      (resolver_for_fixeds_for_app_and_env
       (Just Common.FixFromEnvironmentHasPrecedence)
       (Just (Common.mkEnvFix_gsr theFix_env_value)))
      `shouldGive_forAllCombinationsOfDefaultConfig`
      formBlockRowInfo_gsrMeta theFix_env_value
    ]
  ]

handlingOfDefaultValuesTest :: Test
handlingOfDefaultValuesTest =
  TestList
  [
    "no default" ~:

    (resolver_no_fixes_with_app_and_env_defaults
     Nothing
     Nothing)
    `shouldGive`
    (formBlockRowInfo_widget widgetWithoutDefault)

  , "WHEN there is a Default from Application Configuration " ++
    "(but non in the Env), " ++
    "THEN this value SHOULD be given to the Widget" ~:

    (resolver_no_fixes_with_app_and_env_defaults
     (Just theDefault_app_value)
     Nothing)
    `shouldGive`
    (formBlockRowInfo_widget $ widgetWithDefault theDefault_app_value)

  , "WHEN there is a Default from Environment " ++
    "(but non in the Application Configuration), " ++
    "THEN this value SHOULD be given to the Widget" ~:

    (resolver_no_fixes_with_app_and_env_defaults
     Nothing
     (Just theDefault_env_value))
    `shouldGive`
    (formBlockRowInfo_widget $ widgetWithDefault theDefault_env_value)

  , "WHEN there is a Default both from Environment " ++
    "and from the Application Configuration, " ++
    "THEN the value from the SHOULD be given to the Widget" ~:

    (resolver_no_fixes_with_app_and_env_defaults
     (Just theDefault_app_value)
     (Just theDefault_env_value))
    `shouldGive`
    (formBlockRowInfo_widget $ widgetWithDefault theDefault_env_value)
  ]

resolver_no_fixes_with_app_and_env_defaults :: Maybe AtDefaultType
                                            -> Maybe AtDefaultType
                                            -> Common.FixAndDefaultResolver
                                               AtDefaultType
                                               AtValueType
resolver_no_fixes_with_app_and_env_defaults default_app default_env =
  mkResolver noFixed (DefaultConfiguration default_app default_env)


-------------------------------------------------------------------------------
-- - Test Data -
-------------------------------------------------------------------------------



atTitle :: String
atTitle = "Attribute Title"

-- | Fix value from the Environment used by all tests.
theFix_env_value :: AtValueType
theFix_env_value = "FIX-FROM-ENV"

-- | Default value from the Application used by all tests.
theDefault_app_value :: AtDefaultType
theDefault_app_value = "DEFAULT-FROM-APP"

-- | Default value from the Environment used by all tests.
theDefault_env_value :: AtDefaultType
theDefault_env_value = "DEFAULT-FROM-ENV"

widgetWithoutDefault :: UserInteraction.AnyWIDGET
widgetWithoutDefault = WidgetResources.mkWidget
                       id
                       theAttributeName
                       Nothing
                       theObjectName

widgetWithDefault :: AtDefaultType -> UserInteraction.AnyWIDGET
widgetWithDefault defaultValue = WidgetResources.mkWidget
                       id
                       theAttributeName
                       (Just defaultValue)
                       theObjectName


-------------------------------------------------------------------------------
-- - Test Case utilities -
-------------------------------------------------------------------------------


shouldGive :: Common.FixAndDefaultResolver AtDefaultType AtValueType
           -> UiO.FormBlockRowInfo
           -> Assertion
shouldGive fixAndDefaultResolver expectedFormBlockRowInfo =
  assertEqAttributeOutput
  expectedFormBlockRowInfo
  (mkAtSetup fixAndDefaultResolver)
  ES.empty

-- | Checks the output for a given configuration of fixed values,
-- for all combintions of configuration for default values.
shouldGive_forAllCombinationsOfDefaultConfig :: FixConfiguration AtValueType
                                             -> UiO.FormBlockRowInfo
                                             -> Assertion
shouldGive_forAllCombinationsOfDefaultConfig fixConfig expectedFormBlockRowInfo =
  mapM_ check combinations
  where
    check :: DefaultConfiguration AtDefaultType -> Assertion
    check defaultConfig = (mkResolver fixConfig defaultConfig)
                          `shouldGive`
                          expectedFormBlockRowInfo
    combinations :: [DefaultConfiguration AtDefaultType]
    combinations =
      [
        DefaultConfiguration Nothing Nothing
      , DefaultConfiguration Nothing (Just theDefault_env_value)
      , DefaultConfiguration (Just theDefault_app_value) Nothing
      , DefaultConfiguration (Just theDefault_app_value) (Just theDefault_env_value)
      ]

assertEqAttributeOutput :: UiO.FormBlockRowInfo
                        -> Common.AttributeTypeSetup d a
                        -> ES.ElementSet
                        -> Assertion
assertEqAttributeOutput expected atSetup mediaInEnv =
  TestUiUtils.check
  (TestUiUtils.emptyEnv `TestUiUtils.withMedia` mediaInEnv)
  checkResult
  (Common.attributeOutputer atSetup theObjectName)
  where
    checkResult = AssertUtils.failOnError
                  (FormBlockResources.checkFormBlockRowInfo
                   WidgetResources.checkWidgetByHtmlString
                   FormBlockResources.noCheck
                   "FormBlockRowInfo"
                   expected)


-------------------------------------------------------------------------------
-- - FormBlockRowInfo for the test Attribute -
-------------------------------------------------------------------------------


formBlockRowInfo_gsrMeta :: String -> UiO.FormBlockRowInfo
formBlockRowInfo_gsrMeta =
  UserInteraction.mkFormBlockRowInfoForMetas .
  UserInteractionCommon.metaValuesForRole
  UserInteractionCommon.Fix
  theAttributeName
  theObjectName

formBlockRowInfo_widget :: UserInteraction.AnyWIDGET -> UiO.FormBlockRowInfo
formBlockRowInfo_widget widget =
  UserInteraction.mkFormBlockRowInfoForLabelAndWidget
  (theLabel,widget)

theLabel :: UserInteraction.Label
theLabel =
  UserInteraction.Label
  {
    UserInteraction.labelKey    = theAttributeElementKey
  , UserInteraction.labelString = atTitle
  }


-------------------------------------------------------------------------------
-- - FixAndDefaultResolver for the test Attribute -
-------------------------------------------------------------------------------


resolver_with_no_defaults_but_fixeds_for_app_and_env :: Maybe Common.FixValuePrecedence
                                                     -> Maybe (Common.FixFromEnv AtValueType)
                                                     -> Common.FixAndDefaultResolver AtDefaultType AtValueType
resolver_with_no_defaults_but_fixeds_for_app_and_env mbPrecedence mbFixFromEnv =
  Common.FixAndDefaultResolver
  {
    Common.appResolver =
       Common.FixAndDefaultResolverForApplicationConfiguration
       {
         Common.appFix     = mbPrecedence
       , Common.appDefault = pure Nothing
       }
  , Common.envResolver =
       Common.FixAndDefaultResolverForEnvironment
       {
         Common.envFix     = pure $ mbFixFromEnv
       , Common.envDefault = pure Nothing
       }
  }

resolver_for_fixeds_for_app_and_env :: Maybe Common.FixValuePrecedence
                                    -> Maybe (Common.FixFromEnv AtValueType)
                                    -> FixConfiguration AtValueType
resolver_for_fixeds_for_app_and_env mbPrecedence mbFixFromEnv =
  FixConfiguration
  {
    appFix = mbPrecedence
  , envFix = mbFixFromEnv
  }

mkResolver :: FixConfiguration a
           -> DefaultConfiguration d
           -> Common.FixAndDefaultResolver d a
mkResolver (FixConfiguration fix_app fix_env)
  (DefaultConfiguration default_app default_env)
  =
  Common.FixAndDefaultResolver
  {
    Common.appResolver =
       Common.FixAndDefaultResolverForApplicationConfiguration
       {
         Common.appFix     = fix_app
       , Common.appDefault = pure default_app
       }
  , Common.envResolver =
         Common.FixAndDefaultResolverForEnvironment
         {
           Common.envFix     = pure fix_env
         , Common.envDefault = pure default_env
         }
  }

-- | Configuration of Fix Values.
data FixConfiguration valueType =
  FixConfiguration
  {
    appFix     :: Maybe Common.FixValuePrecedence
  , envFix     :: Maybe (Common.FixFromEnv valueType)
  }
  deriving Show

noFixed :: FixConfiguration a
noFixed =
  FixConfiguration
  {
    appFix = Nothing
  , envFix = Nothing
  }

-- | Configuration of Default Values.
data DefaultConfiguration defaultType =
  DefaultConfiguration
  {
    appDefault :: Maybe defaultType
  , envDefault :: Maybe defaultType
  }
  deriving Show


-------------------------------------------------------------------------------
-- - Object Model -
-------------------------------------------------------------------------------


type AtValueType   = String

type AtDefaultType = String

-- | Name of the object used by all tests.
theObjectName :: UiO.ObjectName
theObjectName = UiO.objectName "object-name"

-- | Name of the attribute used by all tests.
theAttributeName :: UiO.AttributeName
theAttributeName = UiO.attributeName "attribute-name"

-- | Element key of the attribute used by all tests.
theAttributeElementKey :: ES.ElementKey
theAttributeElementKey = UiO.attributeElementKey theObjectName theAttributeName

mkAtSetup :: Common.FixAndDefaultResolver AtDefaultType AtValueType
          -> Common.AttributeTypeSetup    AtDefaultType AtValueType
mkAtSetup fixAndDefaultResolver =
  Common.AttributeTypeSetup
  {
    Common.setupInfo     = attributeTypeInfo
  , Common.setupResolver = \_ _ -> fixAndDefaultResolver
  }

attributeTypeInfo :: Common.AttributeTypeInfo AtDefaultType AtValueType
attributeTypeInfo =
  Common.AttributeTypeInfo
  {
    Common.atiCrossRefKey    = theAttributeName
  , Common.atiTitle          = withNeutralWildeStyle atTitle
  , Common.atiWidgetOutputer = WidgetResources.mkWidgetConstructorGetterForAt id
  , Common.atiGsrOutputer    = id
  }

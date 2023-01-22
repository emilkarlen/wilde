module WildeTest.ObjectModel.UserInteraction.Input.CommonTest
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import qualified Wilde.Media.ElementSet as ES
import qualified Wilde.Media.GenericStringRep as Gsr

import Wilde.ObjectModel.ObjectModel

import qualified Wilde.ObjectModel.UserInteraction.Input.Common as Common
import qualified Wilde.Media.UserInteraction.Input as UiI

import qualified TestResources.Testing.AssertUtils as AssertUtils

import WildeTest.ObjectModel.UserInteraction.SingleAttributeObjectModelTestResources


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theTest :: Test
theTest =
  TestList
  [
    "WHEN no Fixed Value is present, " ++
    "THEN the value from the Widget SHOULD be input" ~:

    (setupWithInputMediaAndWidgetInputer
    ES.empty
    successfullGsrInputer
    (widgetInputerThatGives theWidgetValue)
    )
    `shouldGiveOkResult`
    theWidgetValue

  , "WHEN a Fixed Value is present, " ++
    "THEN this Fixed Value SHOULD be input" ~:

    (setupWithInputMediaAndWidgetInputer
     (ES.fromElements $ metaValuesForFixedGsr theFix_env_value)
     successfullGsrInputer
     (widgetInputerThatGives theWidgetValue)
    )
    `shouldGiveOkResult`
    theFix_env_value

  , "WHEN a Fixed Value is present for a different object, " ++
    "THEN the value from the Widget SHOULD be input" ~:

    (setupWithInputMediaAndWidgetInputer
     (ES.fromElements $
      metaValuesForFixedGsr_forObject
      otherObjectName
      theFix_env_value)
     successfullGsrInputer
     (widgetInputerThatGives theWidgetValue)
    )
    `shouldGiveOkResult`
    theWidgetValue

  , "WHEN a Fixed Value is present but the inputer for it fails, " ++
    "and there is a Widget Inputer that would succeed, " ++
    "THEN the inputer as a whole SHOULD fail" ~:

    (setupWithInputMediaAndWidgetInputer
    (ES.fromElements $ metaValuesForFixedGsr theFix_env_value)
    (gsrInputerThatFailsWith ES.InvalidValue)
    (widgetInputerThatGives theWidgetValue))
    `shouldGiveFailureResult`
    (theAttributeElementKey,ES.InvalidValue,Just theFix_env_value)

  , "WHEN a Fixed Value is not present " ++
    "and there is a Widget Inputer that fails, " ++
    "THEN the inputer as a whole SHOULD fail" ~:

    (setupWithInputMediaAndWidgetInputer
    ES.empty
    successfullGsrInputer
    (widgetInputerThatFailsWith ES.InvalidValue))
    `shouldGiveFailureResult`
    (theAttributeElementKey,ES.InvalidValue,Nothing)
  ]

type WidgetInputer = AttributeName ->
                     UiI.UserInteractionInputer (ES.ElementInputResult AtValueType)

setupWithInputMediaAndWidgetInputer = Setup

data Setup =
  Setup
  {
    setupInputMedia    :: ES.ElementSet
  , setupGsrInputer    ::  Gsr.GenericStringRepInputer AtValueType
  , setupWidgetInputer :: WidgetInputer
  }

shouldGiveOkResult :: Setup -> AtValueType -> Assertion
shouldGiveOkResult setup expectedOkResult =
  do
    result <- run setup
    AssertUtils.failOnError
      checkInputResult
      result
  where
    checkInputResult :: ES.ElementInputResult AtValueType -> Assertion
    checkInputResult = AssertUtils.failOnError
                       (assertEqual "result" expectedOkResult)

shouldGiveFailureResult :: Setup -> ES.ElementLookupError -> Assertion
shouldGiveFailureResult setup expectedFailure =
  do
    result <- run setup
    AssertUtils.failOnError
      checkInputResult
      result
  where
    checkInputResult :: ES.ElementInputResult AtValueType -> Assertion

    checkInputResult (Left actualFailure) =
      assertEqual "error" expectedFailure actualFailure

    checkInputResult (Right success) =
      assertFailure $ "expected error, got success: " ++ show success

run :: Setup -> IO (Either UiI.Error (ES.ElementInputResult AtValueType))
run (Setup inputMedia gsrInputer widgetInputer) = UiI.run env computation
  where
    computation :: UiI.Monad (ES.ElementInputResult AtValueType)
    computation = Common.inputer_fixedFromEnvHasPrecedence
                  gsrInputer
                  widgetInputer
                  theAttributeName
                  theObjectName
    env = UiI.Environment
          {
            UiI.envInputMedia        = inputMedia
          , UiI.envCustomEnvironment = ES.empty
          }


theWidgetValue :: AtValueType
theWidgetValue = "WIDGET-VALUE"

widgetInputerThatGives :: AtValueType
                       -> WidgetInputer
widgetInputerThatGives x attributeName objectName =
  return $ return x

widgetInputerThatFailsWith :: ES.ElementLookupErrorType
                           -> WidgetInputer
widgetInputerThatFailsWith error attributeName objectName =
  return $ Left (theAttributeElementKey,error,Nothing)


successfullGsrInputer :: Gsr.GenericStringRepInputer AtValueType
successfullGsrInputer = return

gsrInputerThatFailsWith :: ES.ElementLookupErrorType
                        -> Gsr.GenericStringRepInputer AtValueType
gsrInputerThatFailsWith error = const $ Left error

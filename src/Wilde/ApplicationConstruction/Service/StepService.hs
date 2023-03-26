-------------------------------------------------------------------------------
-- | Tools for constructing a service that proceeds in steps,
-- where each step requires interaction with the user.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.Service.StepService
       (
         module Wilde.Media.UserInteraction,

         stepService,
         haltWithPage,
         haltWithInformationPopup,
         continue,
         continueTo,
         askIfContinue,
         askIfContinueTo,

         StepService(..),
         NonLastStep,
         ContinueInfo,
         ServicePage,

         StepReference(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Control.Monad

import           Wilde.Utils.Utils

import qualified Wilde.Media.MonadWithInputMedia as MIIA
import qualified Wilde.Media.ElementSet as ElementSet
import           Wilde.Media.UserInteraction

import           Wilde.Render.UserInteractionRendering

import           Wilde.Service.ServiceLink
import qualified Wilde.Application.Service.PopUp as PopUp
import           Wilde.Application.Service.Service

import           Wilde.ApplicationConstruction.Service.ServiceTools
import qualified Wilde.Application.Service.Result as Result

import           Wilde.WildeUi.UiPrimitives (WildeTitle)


-------------------------------------------------------------------------------
-- - Step Service -
-------------------------------------------------------------------------------


-- | A service that is executed in steps, where each step is a screen that is
-- displayed to the user.
data StepService = StepService
                   {
                     mainTitle    :: WildeTitle,
                     nonLastSteps :: [NonLastStep],
                     lastStep     :: Service
                   }

data WhatToDoNext
  = Halt (Either ServicePage InformationPopUpInfo)
  | Continue
    {
      stepReference :: StepReference
      -- | Continue with either a form or a
      -- ask-if-continue popup.
    , pageContent   :: PageContent
    }

type PageContent = Either FormBlocksAndMetas PopUp.Message

-- | Reference to a step in a service.
data StepReference
  = Following
  | LastStep
  | Indexed Int

type NonLastStep  = ServiceMonad WhatToDoNext

type ContinueInfo = FormBlocksAndMetas

type InformationPopUpInfo = PopUp.Message

-- | Makes the service halt with a displayed page.
haltWithPage :: ServicePage -> NonLastStep
haltWithPage = pure . Halt . Left

-- | Makes the service halt with an information popup.
haltWithInformationPopup :: PopUp.Message -> NonLastStep
haltWithInformationPopup = pure . Halt . Right

-- | Makes the service continue to the next step
continue :: FormBlocksAndMetas -> NonLastStep
continue info = continueTo Following info

-- | A variant of 'continue' for with it is possible to
-- determine the next step.
continueTo :: StepReference -> FormBlocksAndMetas -> NonLastStep
continueTo nextStep info =
  pure $
  Continue
  {
    stepReference = nextStep
  , pageContent   = Left info
  }

-- | Shows a message that lets the user continue or not, by selecting
-- "Yes" (continue), or to stop (by doing nothing).
--
-- The purpose of such a step is to inform the user of the consequences
-- of some action that will follow.
askIfContinue :: PopUp.Message -> NonLastStep
askIfContinue msg = askIfContinueTo Following msg

-- | A variant of 'askIfContinue' for with it is possible to
-- determine the next step.
askIfContinueTo :: StepReference -> PopUp.Message -> NonLastStep
askIfContinueTo nextStep msg =
  pure $
  Continue
  {
    stepReference = nextStep
  , pageContent   = Right msg
  }

stepService :: StepService -> Service
stepService (StepService {
                mainTitle    = mainTitle',
                nonLastSteps = nonLastSteps',
                lastStep     = lastStep'
                }) =
  do
    stepIdx <- getStepIdx
    when (stepIdx > lastStepIdx) (throwInvalidStep "step too large")
    if (stepIdx == lastStepIdx)
      then lastStep'
      else
        do
          let step = nonLastSteps' !! stepIdx
          result <- step
          case result of
            Halt     (Left page) -> pageOkResult page
            Halt     (Right msg) -> popupOkResult (Result.informationPopup msg Nothing)
            Continue nextStep content -> do
              nextStepIdx <- getNextStepIdx stepIdx nextStep
              serviceForNextStep content nextStepIdx
     where

       lastStepIdx :: Int
       lastStepIdx = length nonLastSteps'

       serviceForNextStep :: PageContent -> Int -> Service
       serviceForNextStep (Left  x) nextStepIdx = continueWithFormBlocks mainTitle' x nextStepIdx
       serviceForNextStep (Right x) nextStepIdx = askIfContinueWithMsg   mainTitle' x nextStepIdx

       getNextStepIdx :: Int -> StepReference -> ServiceMonad Int
       getNextStepIdx currentStepIdx LastStep    = pure lastStepIdx
       getNextStepIdx currentStepIdx Following   = pureOrThrowIfInvalid (currentStepIdx + 1)
       getNextStepIdx currentStepIdx (Indexed n) = pureOrThrowIfInvalid (n + 1)

       pureOrThrowIfInvalid :: Int -> ServiceMonad Int
       pureOrThrowIfInvalid n = do
         when (n > lastStepIdx) (throwInvalidStep $ "step too large: " ++ show n)
         pure n

       throwInvalidStep :: String -> ServiceMonad a
       throwInvalidStep msg = throwErr $ ValueValue "step" msg

continueWithFormBlocks :: WildeTitle -> ContinueInfo -> Int -> Service
continueWithFormBlocks presSpec formBlocksAndMetas nextStepIdx =
  do
    let formBlocksAndMetas' = setNextStep formBlocksAndMetas
    form      <- formForCurrentService formBlocksAndMetas' []
    component <- toServiceMonad $ formComponent form
    pageOkResult (presSpec,[component])
    where
      setNextStep formBlocksAndMetas = fbamAppendMetas formBlocksAndMetas [nextStepMeta]
      nextStepMeta                   = (globalElementKey varStep,show nextStepIdx)

-- TODO Improve impl, just copying from continueWithFormBlocks.
-- (guess should not need to use forms, a link would as well)
askIfContinueWithMsg :: WildeTitle -> PopUp.Message -> Int -> Service
askIfContinueWithMsg presSpec msg nextStepIdx =
  do
    current <- currentServiceLink
    let nextStep = addGenericParams current [(varStep,show nextStepIdx)]
    popupOkResult $ askIfContinuePopup msg nextStep

varStep :: String
varStep = "_step"

-- | pures a value >= 0.
getStepIdx :: ServiceMonad Int
getStepIdx = MIIA.inInputMedia (lookupStepIdx ek)
  where
    ek            = globalElementKey varStep
    lookupStepIdx = ElementSet.mkLookuper parser
    parser        = ElementSet.singleton_optional >=> parseStepIndex

parseStepIndex :: ElementSet.Parser (Maybe String) Int
parseStepIndex Nothing = Right 0
parseStepIndex (Just stringVal) =
  case readCompletelyAndUnambigously stringVal of
    Nothing -> Left $ ElementSet.InvalidSyntax
    Just n  ->
      if (n < 0)
      then Left $ ElementSet.InvalidSyntax
      else Right n

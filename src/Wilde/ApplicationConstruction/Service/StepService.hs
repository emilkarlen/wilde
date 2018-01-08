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


import Control.Monad

import Wilde.Utils.Utils

import qualified Wilde.Media.MonadWithInputMedia as MIIA
import qualified Wilde.Media.ElementSet as ElementSet
import Wilde.Media.WildeMedia as WM
import Wilde.Media.UserInteraction

import Wilde.Render.UserInteractionRendering

import qualified Wilde.Application.Service as WildeService
import           Wilde.Application.ServiceLink
import qualified Wilde.Application.PopUp as PopUp

import Wilde.ApplicationConstruction.Service.ServiceTools


-------------------------------------------------------------------------------
-- - Step Service -
-------------------------------------------------------------------------------


-- | A service that is executed in steps, where each step is a screen that is
-- displayed to the user.
data StepService = StepService
                   {
                     mainTitle    :: WM.StyledTitle,
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
haltWithPage = return . Halt . Left

-- | Makes the service halt with an information popup.
haltWithInformationPopup :: PopUp.Message -> NonLastStep
haltWithInformationPopup = return . Halt . Right

-- | Makes the service continue to the next step
continue :: FormBlocksAndMetas -> NonLastStep
continue info = continueTo Following info

-- | A variant of 'continue' for with it is possible to
-- determine the next step.
continueTo :: StepReference -> FormBlocksAndMetas -> NonLastStep
continueTo nextStep info =
  return $
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
  return $
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
            Halt     (Right msg) -> popupOkResult (WildeService.informationPopup msg Nothing)
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
       getNextStepIdx currentStepIdx LastStep    = return lastStepIdx
       getNextStepIdx currentStepIdx Following   = returnOrThrowIfInvalid (currentStepIdx + 1)
       getNextStepIdx currentStepIdx (Indexed n) = returnOrThrowIfInvalid (n + 1)
       
       returnOrThrowIfInvalid :: Int -> ServiceMonad Int
       returnOrThrowIfInvalid n = do
         when (n > lastStepIdx) (throwInvalidStep $ "step too large: " ++ show n)
         return n
         
       throwInvalidStep :: String -> ServiceMonad a
       throwInvalidStep msg = throwErr $ ValueValue "step" msg

continueWithFormBlocks :: WM.StyledTitle -> ContinueInfo -> Int -> Service
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
askIfContinueWithMsg :: WM.StyledTitle -> PopUp.Message -> Int -> Service
askIfContinueWithMsg presSpec msg nextStepIdx =
  do
    current <- currentServiceLink
    let nextStep = addGenericParams current [(varStep,show nextStepIdx)]
    popupOkResult $ askIfContinuePopup msg nextStep

varStep :: String
varStep = "_step"

-- | Returns a value >= 0.
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

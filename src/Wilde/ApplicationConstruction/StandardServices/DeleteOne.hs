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

{-# LANGUAGE KindSignatures #-}

-------------------------------------------------------------------------------
-- | The standard service \"Delete One\".
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.StandardServices.DeleteOne
       (
         Config(..),
         Steps(..),
         defaultSteps,
         mkService,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.ObjectModel.ObjectModel

import qualified Wilde.Application.Service as WildeService
import qualified Wilde.Application.PopUp as PopUp

import qualified Wilde.Media.UserInteraction.Output as UiOm
import qualified Wilde.Media.Translations as Translations

import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.GenericStringRep as OmGsr

import Wilde.ApplicationConstruction.Service.ServiceUtils
import Wilde.ApplicationConstruction.Service.ServiceTools
import Wilde.ApplicationConstruction.Service.StepService
import Wilde.Application.ObjectTypeService
import Wilde.ApplicationConstruction.Service.ObjectTypeServiceUtils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data Config (otConf :: * -> * -> * -> * -> *) (atConf :: * -> * -> * -> *) dbTable otNative idAtExisting idAtCreate =
  Config
  {
    title :: StyledTitle
  , steps :: Steps idAtExisting
  }

-- | Steps of the deletion service.
data Steps idAtExisting =
  Steps
  {
    -- | If this computation returns 'Just' a message, then the deletion is
    -- concidered to be impossible, so the operation is halted.
    --
    -- Should not update the information in the database.
    messageIfImpossible             :: idAtExisting -> ServiceMonad (Maybe PopUp.Message)

    -- | If this computation returns 'Just' a message, then the message is displayed
    -- in a ask-if-continue PopUp, which lets the user halt the operation.
    --
    -- Should not update the information in the database.
  , messageIfWarnAboutConsequences  :: idAtExisting -> ServiceMonad (Maybe PopUp.Message)

    -- | Performs the actual deletion in the database.
    --
    -- Returns 'Just' a message if the operation could not be completed.
  , doDeleteAndGiveMessageIfAborted :: idAtExisting -> ServiceMonad (Maybe PopUp.Message)
  }

-- | Default steps.
-- Never concider deletion it impossible.
-- Warns about that the deletion cannot be undone.
defaultSteps :: (Database.DATABASE_TABLE otConf
                ,Database.IO_FOR_EXISTING atConf
                ,Database.COLUMN_NAMES atConf)
             => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
             -> Steps idAtExisting
defaultSteps ot =
  Steps
  {
    messageIfImpossible             = const $ return Nothing
  , messageIfWarnAboutConsequences  = \id ->
      toServiceMonad $ do
        msg <- UiOm.getEnvs $ Translations.trThisIsImpossibleToUndo . UiOm.outTranslations . UiOm.envOutputing
        return $ Just msg
  , doDeleteAndGiveMessageIfAborted = \id -> deleteObject ot id >>
                                             return Nothing
  }

mkService :: OmGsr.ATTRIBUTE_INPUT_FOR_EXISTING atConf
          => [AnyO (OtServiceOtSetup Config otConf atConf)]
          -> AnyOtService
mkService otss = AnyOtService $
  OtService
  {
    main  = AnyObjectTypeServiceMainFunction $ objectIdServiceMainFunction deleteOneMain
  , types = otss
  }

deleteOneMain :: ObjectIdServiceMainFunction Config otConf atConf dbTable otNative idAtExisting idAtCreate
deleteOneMain ot (Config title steps) objectId = stepService def
  where
    def :: StepService
    def = StepService
          {
            mainTitle    = title
          , nonLastSteps = [fromhaltIfImpossible
                           ,askIfcontinueOrDelete]
          , lastStep     = deleteStep
          }

    fromhaltIfImpossible :: NonLastStep
    fromhaltIfImpossible =
      do
        mbHaltMsg <- messageIfImpossible steps objectId
        maybe askIfcontinueOrDelete haltWithInformationPopup mbHaltMsg

    askIfcontinueOrDelete :: NonLastStep
    askIfcontinueOrDelete =
      do
        mbWarnMsg <- messageIfWarnAboutConsequences steps objectId
        case mbWarnMsg of
          Just msg -> askIfContinueTo LastStep msg
          Nothing  -> do
            mbAbortedMsg <- doDeleteAndGiveMessageIfAborted steps objectId
            case mbAbortedMsg of
              Nothing  -> haltWithPage (title,[])
              Just msg -> haltWithInformationPopup msg

    deleteStep :: Service
    deleteStep =
      do
        mbAbortedMsg <- doDeleteAndGiveMessageIfAborted steps objectId
        maybe
          (pageOkResult (title,[]))
          (\msg -> WildeService.popupOkResult
                   (WildeService.informationPopup msg Nothing))
          mbAbortedMsg

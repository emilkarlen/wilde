module Wilde.Application.Service.Result where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Application.Service.PopUp as PopUp
import          Wilde.Service.ServiceLink
import           Wilde.WildeUi.UiPrimitives


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | All types of results of a service.
data ServiceOkResult
  = OkResultPage  ServicePage  -- ^ A simple page with custom contents.
  | OkResultPopUp ServicePopUp -- ^ A small dialog that will "pop up".

-- | Processed all forms of 'ServiceOkResult'.
processOkResult :: (ServicePage -> a)
                -> (ServicePopUp -> a)
                -> ServiceOkResult
                -> a
processOkResult processPage _ (OkResultPage x) = processPage x
processOkResult _ processPopUp (OkResultPopUp x) = processPopUp x

-- | All types of small "pop up" dialogs, possibly with information about how
-- to continue after the user has responded to the choices that the pop up
-- lets the user choose from.
data ServicePopUp
   = AskIfContinue AskIfContinuePopUp
   | Information   InformationPopUp

-- | Processes all kinds of Service "pop up" results.
processPopUpOkResult :: (AskIfContinuePopUp -> a)
                     -> (InformationPopUp -> a)
                     -> ServicePopUp
                     -> a
processPopUpOkResult processAic _ (AskIfContinue x) = processAic x
processPopUpOkResult _ processInf (Information   x) = processInf x

-- | A "pop up" that asks the user wether to continue the service
-- that he/she has started or not.
data AskIfContinuePopUp =
    AskIfContinuePopUp
    {
      askIfContinueMessage      :: PopUp.Message
    , askIfContinueContinuation :: ServiceLink
    }

-- | A "pop up" that gives the user some information.
-- The service might continue or not after the message has been
-- displayed.
data InformationPopUp =
    InformationPopUp
    {
      informationMessage      :: PopUp.Message
    , informationContinuation :: Maybe ServiceLink
    }

askIfContinuePopup :: PopUp.Message -> ServiceLink -> ServicePopUp
askIfContinuePopup msg continuation =
  AskIfContinue (AskIfContinuePopUp msg continuation)

informationPopup :: PopUp.Message -> Maybe ServiceLink -> ServicePopUp
informationPopup msg continuation =
  Information (InformationPopUp msg continuation)

-- | A "page" that is the "result" of the execution of a service.
-- (title,style,contents)
type ServicePage = (WildeTitle
                   ,[AnyCOMPONENT]
                   )

servicePageTitle    :: ServicePage -> Title
servicePageTitle (styledTitle,_) = wildeStyled styledTitle

servicePageStyle    :: ServicePage -> WildeStyle
servicePageStyle (styledTitle,_) = wildeStyle styledTitle

servicePageContents :: ServicePage -> [AnyCOMPONENT]
servicePageContents (_,c) = c

-- | A component made up of a data/info component followed by a button row.

module Wilde.ApplicationConstruction.Presentation.DataAndButtonsComponent
(
    new,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Render.Html.Element as HE

import           Wilde.WildeUi.WildeValue (AnySVALUE, SVALUE(..))
import qualified Wilde.WildeUi.WildeStyles as WS

import qualified Wilde.GenericUi.Component as Comp

import qualified Wilde.Media.Presentation as Presentation


import qualified Wilde.Utils.TextHtmlUtils as HU

import qualified Wilde.ApplicationConstruction.Presentation.ButtonSequenceValue as BtnSeq
import           Wilde.WildeUi.UiPrimitives


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


new :: AnyCOMPONENT -- ^ data / contents
    -> [AnySVALUE]  -- ^ buttons
    -> AnyCOMPONENT
new data_ buttons = AnyCOMPONENT $ DataAndButtonsComponent data_ buttons

data DataAndButtonsComponent =
    DataAndButtonsComponent
    {
        data_   :: AnyCOMPONENT
    ,   buttons :: [AnySVALUE]
    }

instance COMPONENT DataAndButtonsComponent where
  componentHtml (DataAndButtonsComponent data_ buttons) = componentHtml comp
    where
        comp        :: AnyCOMPONENT
        comp         = Comp.sequenceComponent $ data_ : buttonsComp
        buttonsComp :: [AnyCOMPONENT]
        buttonsComp  = mbButtonsComponent buttons

-- | Gives an empty or singleton list.
getMbButtonComponent
  :: [Presentation.Monad AnySVALUE]
  -> Presentation.Monad [AnyCOMPONENT]
getMbButtonComponent mkButtons = do
  buttonValues <- sequence mkButtons
  pure $ mbButtonsComponent buttonValues

-- | Gives an empty or singleton list.
mbButtonsComponent :: [AnySVALUE] -> [AnyCOMPONENT]
mbButtonsComponent []      = []
mbButtonsComponent buttons = [AnyCOMPONENT $ ButtonsRowComponent buttons]

newtype ButtonsRowComponent = ButtonsRowComponent [AnySVALUE]

instance COMPONENT ButtonsRowComponent where
  componentHtml (ButtonsRowComponent buttons) =
    HU.withclasses WS.actionsComponentClasses $ HE.div elements
    where
      elements :: HE.Html
      elements = valueHtmlStyled $ BtnSeq.new buttons

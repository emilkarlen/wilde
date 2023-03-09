-- | A component made up of a data/info component followed by a button row.

module Wilde.Render.DataAndButtonsComponent
(
    new,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Render.Html.Element as HE

import           Wilde.Media.WildeValue (AnySVALUE, SVALUE(..))
import qualified Wilde.Media.WildeStyle as WS

import qualified Wilde.WildeUi.LayoutValues as LayoutValues
import qualified Wilde.WildeUi.LayoutComponents  as LayoutComponents
import qualified Wilde.GenericUi.Component as Comp

import qualified Wilde.Media.Presentation as Presentation

import           Wilde.ObjectModel.ObjectModel

import qualified Wilde.Utils.TextHtmlUtils as HU


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
    HU.withclasses WS.buttonsComponentClasses $ HE.div buttonsHtml
    where
      buttonsHtml = HE.seq $ map valueHtmlStyled buttons :: HE.Html
      component = LayoutComponents.svalueComponent $
                              LayoutValues.horizontal
                              buttons

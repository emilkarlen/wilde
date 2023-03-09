-- | A SVALUE representing a sequence/line/row of buttons

module Wilde.ApplicationConstruction.Presentation.ButtonSequenceValue
(
    new,
    newV,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.List (intersperse)

import qualified Wilde.Render.Html.Element as HE

import           Wilde.Media.WildeValue (AnySVALUE, SVALUE(..), VALUE(..), withNeutralStyleAny, AnyVALUE)
import qualified Wilde.Media.WildeStyle as WS

import qualified Wilde.Utils.TextHtmlUtils as HU


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


new :: [AnySVALUE]  -- ^ buttons
    -> AnySVALUE
new = withNeutralStyleAny . ButtonsSvalue

newV :: [AnyVALUE]  -- ^ buttons
     -> AnySVALUE
newV = new . map withNeutralStyleAny

newtype ButtonsSvalue = ButtonsSvalue [AnySVALUE]

instance VALUE ButtonsSvalue where
  valueHtml (ButtonsSvalue buttons) = HE.seq $ intersperse btnSepa buttonsHtml
    where
      buttonsHtml :: [HE.Html]
      buttonsHtml  = map valueHtmlStyled buttons

btnSepa :: HE.Html
btnSepa = HU.withclasses WS.buttonsSepaClasses (HE.span HE.empty)

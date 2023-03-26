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


import qualified Wilde.Render.Html.Element as HE

import           Wilde.WildeUi.WildeValue (AnySVALUE, SVALUE(..), VALUE(..), withNeutralStyleAny, AnyVALUE)


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
  valueHtml (ButtonsSvalue buttons) = HE.seq buttonsHtml
    where
      buttonsHtml :: [HE.Html]
      buttonsHtml  = map valueHtmlStyled buttons

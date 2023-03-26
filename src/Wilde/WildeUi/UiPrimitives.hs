-- | "Primitive" UI stuff.
--
-- "Primitive" means that dependencies should be few.
--
module Wilde.WildeUi.UiPrimitives
(
  module Wilde.Utils.Empty,
  module Wilde.GenericUi.Style,
  module Wilde.GenericUi.Component,
  module Wilde.WildeUi.WildeStyleType,

  -- * Titles

  Title,
  WildeTitle,
  neutralTitle,

  -- * Styled elements

  ElementWithStyle(..),
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Utils.Empty

import           Wilde.Render.Html.Types
import qualified Wilde.Render.Html.Element as HE

import           Wilde.GenericUi.Component
import           Wilde.GenericUi.Style

import           Wilde.WildeUi.WildeValue
import           Wilde.WildeUi.WildeStyleType


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - titles -
-------------------------------------------------------------------------------


type Title      = String

type WildeTitle = WildeStyling Title

neutralTitle :: Title -> WildeTitle
neutralTitle = withNeutralWildeStyle


-------------------------------------------------------------------------------
-- - ElementWithStyle -
-------------------------------------------------------------------------------


-- | An UI element with style - primarily for table cells.
--
-- TODO: Replace SeHtml with something representation independent.
data ElementWithStyle = SeValue AnySVALUE
                      | SeHtml  Html

instance VALUE ElementWithStyle where
    valueHtml (SeValue (AnySVALUE x)) = valueHtml x
    valueHtml (SeHtml  html)          = html

instance SVALUE ElementWithStyle where
    valueStyle (SeValue (AnySVALUE x)) = valueStyle x
    valueStyle (SeHtml _)              = neutral

instance EMPTY ElementWithStyle where
    empty = SeHtml HE.empty

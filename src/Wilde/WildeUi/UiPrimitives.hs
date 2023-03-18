-- | "Primitive" UI stuff.
--
-- "Primitive" means that dependencies should be few.
--
module Wilde.WildeUi.UiPrimitives
       (
         module Wilde.Utils.Empty,
         module Wilde.GenericUi.Style,
         module Wilde.Media.WildeStyleType,
         module Wilde.GenericUi.Component,

         -- * Styled elements

         ElementWithStyle(..),

         -- * Wilde tables
         WildeTable,
         WildeRowGroup,
         WildeRow,
         WildeCell,

         wildeCellFromSVALUE,

       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Utils.Empty

import           Wilde.Render.Html.Types
import qualified Wilde.Render.Html.Element as HE

import Wilde.GenericUi.AbstractTable

import Wilde.GenericUi.Component
import Wilde.Media.WildeValue

import Wilde.GenericUi.Style
import Wilde.Media.WildeStyleType


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


-------------------------------------------------------------------------------
-- - WildeTable -
-------------------------------------------------------------------------------


type WildeTable      = StyledTable    WildeStyle AnyVALUE
type WildeRowGroup   = StyledRowGroup WildeStyle AnyVALUE
type WildeRow        = StyledRow      WildeStyle AnyVALUE
type WildeCell       = StyledCell     WildeStyle AnyVALUE

wildeCellFromSVALUE :: SVALUE a => CellType -> Span -> a -> WildeCell
wildeCellFromSVALUE cellType span svalue =
    conCell (valueStyle svalue) cellType span (AnyVALUE svalue)

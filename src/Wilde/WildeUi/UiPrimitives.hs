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
         WildeStyledCell,
         
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Utils.Empty

import Text.Html

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
    empty = SeHtml noHtml


-------------------------------------------------------------------------------
-- - WildeTable -
-------------------------------------------------------------------------------


type WildeTable      = StyledTable    WildeStyle AnyVALUE
type WildeRowGroup   = StyledRowGroup WildeStyle AnyVALUE
type WildeRow        = StyledRow      WildeStyle AnyVALUE
type WildeStyledCell = StyledCell     WildeStyle AnyVALUE

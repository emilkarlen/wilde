-- | Definition of Wilde Table Structure

module Wilde.WildeUi.WildeTable
(
  WildeTable,
  WildeRowGroup,
  WildeRow,
  WildeCell,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.GenericUi.AbstractTable
import           Wilde.WildeUi.WildeValue


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type WildeTable      = StyledTable    WildeStyle AnyVALUE
type WildeRowGroup   = StyledRowGroup WildeStyle AnyVALUE
type WildeRow        = StyledRow      WildeStyle AnyVALUE
type WildeCell       = StyledCell     WildeStyle AnyVALUE

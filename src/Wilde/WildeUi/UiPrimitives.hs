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
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Utils.Empty

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

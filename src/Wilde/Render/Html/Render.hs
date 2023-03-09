-- | Transformation of HTML to string.

module Wilde.Render.Html.Render
(
    standard,
    pretty,
)
where

-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Text.Blaze.Html.Renderer.String as RS
import qualified Text.Blaze.Html.Renderer.Pretty as RP

import           Wilde.Render.Html.Types


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


standard, pretty :: Html -> String

standard = RS.renderHtml
pretty   = RP.renderHtml

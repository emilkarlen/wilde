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


import qualified Text.Html as H

import           Wilde.Render.Html.Types


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


standard, pretty :: Html -> String

standard = H.renderHtml
pretty   = H.prettyHtml

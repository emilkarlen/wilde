-- | Styling of HTML.
module Wilde.Render.StyleForHtml where

import Wilde.Render.Html.Types
import Wilde.GenericUi.Style

-- | A style that can be applied to HTML.
class STYLE a => STYLE_FOR_HTML a where
    applyStyleToHtml :: a -> Html -> Html

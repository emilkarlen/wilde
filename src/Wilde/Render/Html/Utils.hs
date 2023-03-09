-- | Utilities related to HTML.
module Wilde.Render.Html.Utils
(
    rootElemOrEmpty,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------

import Text.Blaze.XHtml5 (Html)
import Text.Blaze.Internal (MarkupM(..))


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Gives a root element for some html, or Nothing, if the html is empty or a comment
--
-- Note: This function is not 100% exact - it depends on the internals of
-- Text.Blaze and does its best to interpret the internal structure!
--
-- If the html consists of plain text or is a sequence of elements,
-- then the html is wrapped using a given wrapper that must
-- give the html wrapped in a single element.
--
-- If the html has no contents, Nothing is given.
rootElemOrEmpty :: (Html -> Html) -> Html -> Maybe Html
rootElemOrEmpty wrapInSingleRootElement html
  | hasContents html = Just $
    case html of
      Content {} -> wrapInSingleRootElement html
      Append {}  -> wrapInSingleRootElement html
      _          -> html
  | otherwise = Nothing

hasContents :: MarkupM a -> Bool
hasContents (Empty {})   = False
hasContents (Comment {}) = False
hasContents (Append a b) = hasContents a || hasContents b
hasContents _            = True

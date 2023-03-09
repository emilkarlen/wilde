{-# LANGUAGE Strict #-}

{- |
Almost 1-to-1 wrapping of html generation - to remove dependency on
concrete html types.

"Almost 1-to-1" menans that there is no intention to wrap the concrete
implementation in an implementation dependent way - changes to this module
to fit the concrete implementation are accepted.

Makes it easier to change the concrete html representation.

Introduced for switching html representation - may not be
of less use after that.
-}

module Wilde.Render.Html.Types
(
    Label,

    DomEvent(..),
    JavaScriptProgram,

    URL,

    HtmlAttr,
    Html,

    HTML(..),
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Text.Blaze.XHtml5 as H


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------

type Label = String

data DomEvent = OnClick
                deriving (Show,Read,Eq,Ord,Enum,Bounded)

-- | A program in JavaScript.
type JavaScriptProgram = String

type URL = String

type HtmlAttr = H.Attribute

type Html = H.Html

-- class H.ToMarkup a => HTML a where
class HTML a where
    toHtml :: a -> Html

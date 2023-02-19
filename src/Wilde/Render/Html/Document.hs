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

IMPORT QUALIFIED
-}

module Wilde.Render.Html.Document
(
    module Wilde.Render.Html.Types,

    Document,

    document,
    title,
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


type Document = H.Html

document
    :: Html -- ^ head contents
    -> Html -- ^ body contents
    -> (Html -> Html) -- ^ body element transformer, for setting attributes
    -> Document
document headContents bodyContents transBody = H.thehtml $ head H.+++ body
    where
        head = H.header headContents :: Html
        body = transBody $ H.body bodyContents :: Html


title :: String -> Html
title = H.thetitle . H.stringToHtml

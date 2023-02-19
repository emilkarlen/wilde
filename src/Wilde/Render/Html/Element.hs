{-# LANGUAGE Strict #-}

{- |
1-to-1 wrapping of html generation - to remove dependency on
concrete html types.

"Almost 1-to-1" menans that there is no intention to wrap the concrete
implementation in an implementation dependent way - changes of this module
to fit the concrete implementation are accepted.

Makes it easier to change the concrete html representation.

Introduced for switching html representation - may not be
of less use after that.

IMPORT QUALIFIED
-}

module Wilde.Render.Html.Element
(
    module Wilde.Render.Html.Types,

    empty,
    seq,
    str,
    htmlString,

    withAttrs,

    div,
    span,

    anchor,
    link,

    -- * Tables

    table,
    thead,
    tbody,
    tfoot,
    tr,
    th,
    td,

    -- * Forms

    form,
    reset,
    input,
    submit,

    -- * Misc

    br,
    image,

    center,
    paragraph,

    textarea,
    select,
    option,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Prelude hiding (seq, div, span)

import qualified Text.Html as H

import           Wilde.Render.Html.Types


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


empty :: Html
empty = H.noHtml

seq :: [Html] -> Html
seq = foldl (H.+++) empty

str :: String -> Html
str = H.stringToHtml

-- | A string that is valid html
htmlString :: String -> Html
htmlString = H.primHtml

withAttrs :: Html -> [HtmlAttr] -> Html
withAttrs = (H.!)

anchor :: Html -> Html
anchor = H.anchor

link :: Html
link = H.tag "LINK" empty

div :: Html -> Html
div = H.thediv

span :: Html -> Html
span = H.thespan

-- Tables

table :: Html -> Html
table = H.table

thead :: Html -> Html
thead = H.tag "THEAD"

tbody :: Html -> Html
tbody = H.tag "TBODY"

tfoot :: Html -> Html
tfoot = H.tag "TFOOT"

tr, th, td :: Html -> Html
tr = H.tr
th = H.th
td = H.td

-- Forms

form :: Html -> Html
form = H.form

reset :: String -> String -> Html
reset = H.reset

input :: Html
input = H.input

submit :: String -> String -> Html
submit = H.submit

-- Misc

image :: Html
image = H.image

center :: Html -> Html
center = H.center

br :: Html
br = H.br

paragraph :: Html -> Html
paragraph = H.paragraph

textarea :: Html -> Html
textarea = H.textarea

select, option :: Html -> Html
select = H.select
option = H.option

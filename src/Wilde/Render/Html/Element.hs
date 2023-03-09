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

    nbsp,
    br,
    image,

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

import qualified Text.Blaze.XHtml5 as H

import qualified Wilde.Render.Html.Attribute as RHA

import           Wilde.Render.Html.Types


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


empty :: Html
empty = mempty

seq :: [Html] -> Html
seq = mconcat

-- | Unquoted string
str :: String -> Html
str = H.toHtml

-- | A string that is valid html
htmlString :: String -> Html
htmlString = H.preEscapedToHtml

withAttrs :: Html -> [HtmlAttr] -> Html
withAttrs elem = foldl (H.!) elem

anchor :: Html -> Html
anchor = H.a

link :: Html
link = H.link

div :: Html -> Html
div = H.div

span :: Html -> Html
span = H.span

-- Tables

table :: Html -> Html
table = H.table

thead :: Html -> Html
thead = H.thead

tbody :: Html -> Html
tbody = H.tbody

tfoot :: Html -> Html
tfoot = H.tfoot

tr, th, td :: Html -> Html
tr = H.tr
th = H.th
td = H.td

-- Forms

form :: Html -> Html
form = H.form

input :: Html
input = H.input

reset :: Label -> Html
reset lbl = input `withAttrs` [RHA.type_ "reset", RHA.value lbl]

submit :: Label -> Html
submit lbl = input `withAttrs` [RHA.type_ "submit", RHA.value lbl]

-- Misc

nbsp :: Html
nbsp  = H.preEscapedToMarkup "&nbsp;"

image :: Html
image = H.img

br :: Html
br = H.br

paragraph :: Html -> Html
paragraph = H.p

textarea :: Html -> Html
textarea = H.textarea

select, option :: Html -> Html
select = H.select
option = H.option

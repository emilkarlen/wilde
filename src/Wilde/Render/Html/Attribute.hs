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

module Wilde.Render.Html.Attribute
(
    module Wilde.Render.Html.Types,

    class_,
    domEvent,

    custom,
    rel,
    src,
    href,

    size,

    colspan,
    rowspan,
    cols,
    rows,

    type_,
    type_hidden,
    value,
    method,
    action,
    accesskey,
    readonly,
    checked,
    selected,

    name,
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


custom :: String -> String -> HtmlAttr
custom = H.HtmlAttr

domEvent :: DomEvent -> JavaScriptProgram -> HtmlAttr
domEvent de = custom $ show de

class_:: String -> HtmlAttr
class_ = H.theclass

rel :: String -> HtmlAttr
rel = H.rel

src :: String -> HtmlAttr
src = H.src

href :: String -> HtmlAttr
href = H.href

size :: Int -> HtmlAttr
size = H.size . show

colspan, rowspan :: Int -> HtmlAttr
colspan = H.colspan
rowspan = H.rowspan

cols, rows :: Int -> HtmlAttr
cols = H.cols . show
rows = H.rows . show

type_, value, method, action :: String -> HtmlAttr

type_ = H.thetype
value = H.value
method = H.method
action = H.action

readonly, checked, selected :: HtmlAttr
readonly = custom "readonly" "readonly"
checked = H.checked
selected = H.selected

type_hidden :: HtmlAttr
type_hidden = H.thetype "hidden"

name :: String -> HtmlAttr
name = H.name

accesskey :: Char -> HtmlAttr
accesskey ch = H.HtmlAttr "accesskey" [ch]

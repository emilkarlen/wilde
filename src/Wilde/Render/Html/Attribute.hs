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
    style,
    domEvent,

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


import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as HA

import           Wilde.Render.Html.Types


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


domEvent :: DomEvent -> JavaScriptProgram -> HtmlAttr
domEvent OnClick  = HA.onclick . H.stringValue

class_:: String -> HtmlAttr
class_ = HA.class_ . H.stringValue

style:: String -> HtmlAttr
style = HA.style . H.stringValue

rel :: String -> HtmlAttr
rel = HA.rel . H.stringValue

src :: String -> HtmlAttr
src = HA.src . H.stringValue

href :: String -> HtmlAttr
href = HA.href . H.stringValue

size :: Int -> HtmlAttr
size = HA.size .  H.stringValue . show

colspan, rowspan :: Int -> HtmlAttr
colspan = HA.colspan . H.stringValue . show
rowspan = HA.rowspan . H.stringValue . show

cols, rows :: Int -> HtmlAttr
cols = HA.cols . H.stringValue . show
rows = HA.rows . H.stringValue . show

type_, value, method, action :: String -> HtmlAttr

type_  = HA.type_ . H.stringValue
value  = HA.value . H.stringValue
method = HA.method . H.stringValue
action = HA.action . H.stringValue

readonly, checked, selected :: HtmlAttr
readonly = HA.readonly true
checked  = HA.checked true
selected = HA.selected true

type_hidden :: HtmlAttr
type_hidden = HA.type_ $ H.stringValue "hidden"

name :: String -> HtmlAttr
name = HA.name . H.stringValue

accesskey :: Char -> HtmlAttr
accesskey ch = HA.accesskey $ H.stringValue [ch]

true :: H.AttributeValue
true = H.stringValue "true"
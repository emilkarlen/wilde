-- | Usefull values of types defined in Values and StdValueTypes.
module Wilde.WildeUi.StdValues where

import Wilde.WildeUi.WildeValue
import Wilde.WildeUi.StdValueTypes

vSpace :: AnyVALUE
vSpace = AnyVALUE $ UnquotedStringValue " "

vhSpace :: ValueWithHiddenStyle
vhSpace = hideStyle $ UnquotedStringValue " "

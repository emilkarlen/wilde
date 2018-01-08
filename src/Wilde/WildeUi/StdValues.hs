{-
Copyright 2013 Emil Karlén.

This file is part of Wilde.

Wilde is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Wilde is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Wilde.  If not, see <http://www.gnu.org/licenses/>.
-}

-- | Usefull values of types defined in Values and StdValueTypes.
module Wilde.WildeUi.StdValues where

import Wilde.Media.WildeValue
import Wilde.WildeUi.StdValueTypes

vSpace :: AnyVALUE
vSpace = AnyVALUE $ UnquotedStringValue " "

vhSpace :: ValueWithHiddenStyle
vhSpace = hideStyle $ UnquotedStringValue " "

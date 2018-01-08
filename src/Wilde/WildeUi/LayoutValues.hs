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

-- | Types used to layout values.
module Wilde.WildeUi.LayoutValues
       (
         horizontal,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Text.Html

import Wilde.Media.WildeValue


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


horizontal :: SVALUE a => [a] -> AnySVALUE
horizontal = AnySVALUE . Horizontal

newtype Horizontal a = Horizontal [a]

instance VALUE a => VALUE (Horizontal a) where
  valueHtml   (Horizontal []) = noHtml
  valueHtml   (Horizontal xs) = concatHtml $ map valueHtml xs
  -- valueHtml   (Horizontal xs) = concatHtml $ map valueHtmlStyled xs
  valueString (Horizontal xs) = concatMap valueString xs
  
instance SVALUE a => SVALUE (Horizontal a)

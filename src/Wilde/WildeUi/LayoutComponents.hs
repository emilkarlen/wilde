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

-- | Components for laying out pages.
module Wilde.WildeUi.LayoutComponents
       (
         module Wilde.WildeUi.UiPrimitives,
         module Wilde.Media.WildeValue,
         
         verticalComponents,
         horizontalComponents,
         
         svalueComponent,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.List

import qualified Text.Html  as H hiding (HtmlTable)

import Wilde.WildeUi.UiPrimitives
import Wilde.Media.WildeValue


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - vertical -
-------------------------------------------------------------------------------


-- | Constructs a component that is a vertical list of sub components.
verticalComponents :: COMPONENT a => [a] -> AnyCOMPONENT
verticalComponents xs = AnyCOMPONENT $ Vertical xs

newtype Vertical a = Vertical [a]

instance COMPONENT a => COMPONENT (Vertical a) where
    componentHtml (Vertical xs) = H.concatHtml $ intersperse H.br $ map componentHtml xs


-------------------------------------------------------------------------------
-- - horizontal -
-------------------------------------------------------------------------------


-- | Constructs a component that is a horizontal list of sub components.
horizontalComponents :: COMPONENT a => [a] -> AnyCOMPONENT
horizontalComponents xs = AnyCOMPONENT $ Horizontal xs

newtype Horizontal a = Horizontal [a]

instance COMPONENT a => COMPONENT (Horizontal a) where
    componentHtml (Horizontal xs) = H.concatHtml $ map componentHtml xs


-------------------------------------------------------------------------------
-- - svalueComponent -
-------------------------------------------------------------------------------

-- | A component that is a single 'SVALUE'.
svalueComponent :: SVALUE a => a -> AnyCOMPONENT
svalueComponent = AnyCOMPONENT . SvalueComponent
  
newtype SvalueComponent a = SvalueComponent a

instance VALUE a => VALUE (SvalueComponent a) where
  valueHtml   (SvalueComponent x) = valueHtml x
  valueString (SvalueComponent x) = valueString x

instance SVALUE a => SVALUE (SvalueComponent a)

instance SVALUE a => COMPONENT (SvalueComponent a) where
    componentHtml (SvalueComponent x) = valueHtml x

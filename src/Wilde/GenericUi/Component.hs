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

{-# LANGUAGE ExistentialQuantification #-}

module Wilde.GenericUi.Component
       (
         COMPONENT(..),
         
         HtmlOnly(..),
         
         AnyCOMPONENT(..),
         anyComponent,
         
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Text.Html

import Wilde.Utils.Empty


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - COMPONENT -
-------------------------------------------------------------------------------


-- | A GUI page consists of a sequence of components.
class COMPONENT a where
    componentHtml :: a -> Html


-------------------------------------------------------------------------------
-- - HtmlOnly -
-------------------------------------------------------------------------------


data HtmlOnly = HtmlOnly Html

instance COMPONENT HtmlOnly where
  componentHtml (HtmlOnly html) = html


-------------------------------------------------------------------------------
-- - empty -
-------------------------------------------------------------------------------


-- | () is the empty component.
instance COMPONENT () where
    componentHtml _ = noHtml
  
instance EMPTY AnyCOMPONENT where
  empty = AnyCOMPONENT ()
    

-------------------------------------------------------------------------------
-- - AnyCOMPONENT -
-------------------------------------------------------------------------------


-- | Makes an 'AnyCOMPONENT' from a 'COMPONENT'.
anyComponent :: COMPONENT a => a -> AnyCOMPONENT
anyComponent a = AnyCOMPONENT a

data AnyCOMPONENT = forall a . COMPONENT a => AnyCOMPONENT a

instance COMPONENT AnyCOMPONENT where
    componentHtml (AnyCOMPONENT x) = componentHtml x

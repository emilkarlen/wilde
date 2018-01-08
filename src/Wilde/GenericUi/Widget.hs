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

-- | Defines widget - an element in User Interaction Media for letting
-- the user inputing information to the application.
module Wilde.GenericUi.Widget
       (
         -- * Widgets
         
         WIDGET(..),
         AnyWIDGET(..),
         
         -- * Items in widgets that handle multiple items
         
         MultiItem,
         MultiItemPresentation,

         -- * Labels for widgets
         
         Label(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Text.Html as Html

import Wilde.GenericUi.Value

import Wilde.Media.Element
import Wilde.Media.GenericStringRep


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------
  


-------------------------------------------------------------------------------
-- - Label -
-------------------------------------------------------------------------------


-- | A label in a 'Form'.
--
-- Typically, describes to the user the role of a 'Widget'.
data Label = Label
             {
               labelKey    :: ElementKey
             , labelString :: String
             }
           deriving (Eq,Show)


-------------------------------------------------------------------------------
-- - Widget -
-------------------------------------------------------------------------------


class WIDGET a where
  widgetHtml :: a -> Html.Html
  
data AnyWIDGET = forall a . WIDGET a => AnyWIDGET a

instance VALUE AnyWIDGET where   
  valueHtml (AnyWIDGET x) = widgetHtml x


-------------------------------------------------------------------------------
-- - MultiItem -
-------------------------------------------------------------------------------


-- | An item that is part of a set/list of items.
--
-- (ID,presentation string).
type MultiItem             = (GenericStringRep,MultiItemPresentation)
type MultiItemPresentation = AnyVALUE

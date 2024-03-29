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

         LabelString,
         Label(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Render.Html.Types hiding (Label)

import Wilde.GenericUi.Value

import Wilde.Media.Element
import Wilde.Media.GenericStringRep


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Label -
-------------------------------------------------------------------------------


type LabelString = String

-- | A label in a 'Form'.
--
-- Typically, describes to the user the role of a 'Widget'.
data Label = Label
             {
               labelKey    :: ElementKey
             , labelString :: LabelString
             }
           deriving (Eq,Show)


-------------------------------------------------------------------------------
-- - Widget -
-------------------------------------------------------------------------------


class WIDGET a where
  widgetHtml :: a -> Html

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

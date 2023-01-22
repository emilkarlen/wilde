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

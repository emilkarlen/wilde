{-# LANGUAGE ExistentialQuantification #-}

module Wilde.GenericUi.Component
       (
         COMPONENT(..),

         sequenceComponent,
         HtmlOnly(..),

         AnyCOMPONENT(..),
         anyComponent,

       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Render.Html.Element as HE

import Wilde.Utils.Empty


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - COMPONENT -
-------------------------------------------------------------------------------


-- | A GUI page consists of a sequence of components.
class COMPONENT a where
    componentHtml :: a -> HE.Html


-------------------------------------------------------------------------------
-- - HtmlOnly -
-------------------------------------------------------------------------------


newtype HtmlOnly = HtmlOnly HE.Html

instance COMPONENT HtmlOnly where
  componentHtml (HtmlOnly html) = html


-------------------------------------------------------------------------------
-- - empty -
-------------------------------------------------------------------------------


-- | () is the empty component.
instance COMPONENT () where
    componentHtml _ = HE.empty

instance EMPTY AnyCOMPONENT where
  empty = AnyCOMPONENT ()


-------------------------------------------------------------------------------
-- - sequence -
-------------------------------------------------------------------------------


-- | Constructs a component that is a sequence components.
sequenceComponent :: COMPONENT a => [a] -> AnyCOMPONENT
sequenceComponent xs = AnyCOMPONENT $ Sequence xs

newtype Sequence a = Sequence [a]

instance COMPONENT a => COMPONENT (Sequence a) where
    componentHtml (Sequence xs) = HE.seq $ map componentHtml xs


-------------------------------------------------------------------------------
-- - AnyCOMPONENT -
-------------------------------------------------------------------------------


-- | Makes an 'AnyCOMPONENT' from a 'COMPONENT'.
anyComponent :: COMPONENT a => a -> AnyCOMPONENT
anyComponent a = AnyCOMPONENT a

data AnyCOMPONENT = forall a . COMPONENT a => AnyCOMPONENT a

instance COMPONENT AnyCOMPONENT where
    componentHtml (AnyCOMPONENT x) = componentHtml x

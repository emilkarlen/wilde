{-# LANGUAGE ExistentialQuantification #-}

module Wilde.GenericUi.Value
       (
         module Wilde.Utils.Empty,

         VALUE(..),

         AnyVALUE(..),

         ValueEmpty(..),

         valueEmpty,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Text.Html as H

import           Wilde.Render.Html.Types ( HTML(..), Html )
import qualified Wilde.Render.Html.Element as HE

import Wilde.Utils.Empty


-------------------------------------------------------------------------------
-- - VALUE -
-------------------------------------------------------------------------------

-- | An "unstyled" values that can be rendered on all the supported output
-- formats.
class VALUE a where
    valueHtml   :: a -> Html
    valueString :: a -> String

    valueString = const ""

instance VALUE a => VALUE (Maybe a) where
    valueHtml Nothing  = HE.empty
    valueHtml (Just x) = valueHtml x

instance (VALUE a,VALUE b) => VALUE (Either a b) where
    valueHtml (Left  x) = valueHtml x
    valueHtml (Right y) = valueHtml y

instance VALUE a => VALUE ([] a) where
    valueHtml xs = HE.seq $ map valueHtml xs


-------------------------------------------------------------------------------
-- - AnyVALUE -
-------------------------------------------------------------------------------


-- | Type for heterougenous collections of values.
data AnyVALUE = forall a . VALUE a => AnyVALUE a

instance VALUE AnyVALUE where
    valueHtml (AnyVALUE x) = valueHtml x

instance H.HTML AnyVALUE where
    toHtml = valueHtml

instance HTML AnyVALUE where
    toHtml = valueHtml

instance Show AnyVALUE where
  show = valueString

-- | So that we can let AnyVALUE instantiate EMPTY.
newtype ValueEmpty = ValueEmpty ()

instance VALUE ValueEmpty where
    valueHtml _ = HE.empty

instance EMPTY AnyVALUE where
    empty = AnyVALUE $ ValueEmpty ()

-- | Shortcut to avoid type clashes with empty :: AnySVALUE.
valueEmpty :: AnyVALUE
valueEmpty = empty

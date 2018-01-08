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


import Text.Html

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
    valueHtml Nothing  = noHtml
    valueHtml (Just x) = valueHtml x

instance (VALUE a,VALUE b) => VALUE (Either a b) where
    valueHtml (Left  x) = valueHtml x
    valueHtml (Right y) = valueHtml y

instance VALUE a => VALUE ([] a) where
    valueHtml xs = concatHtml $ map valueHtml xs


-------------------------------------------------------------------------------
-- - AnyVALUE -
-------------------------------------------------------------------------------


-- | Type for heterougenous collections of values.
data AnyVALUE = forall a . VALUE a => AnyVALUE a

instance VALUE AnyVALUE where
    valueHtml (AnyVALUE x) = valueHtml x

instance HTML AnyVALUE where
    toHtml = valueHtml

instance Show AnyVALUE where
  show = valueString

-- | So that we can let AnyVALUE instantiate EMPTY.
newtype ValueEmpty = ValueEmpty ()

instance VALUE ValueEmpty where
    valueHtml _ = noHtml
                
instance EMPTY AnyVALUE where
    empty = AnyVALUE $ ValueEmpty ()

-- | Shortcut to avoid type clashes with empty :: AnySVALUE.
valueEmpty :: AnyVALUE
valueEmpty = empty

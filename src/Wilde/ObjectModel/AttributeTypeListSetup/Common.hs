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

-------------------------------------------------------------------------------
-- | Functionality common for serveral Attribute Type Setups.
-------------------------------------------------------------------------------
module Wilde.ObjectModel.AttributeTypeListSetup.Common
       (
         toMkResult2GeneralMonad,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.ObjectModel.ObjectModel


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Helper method for constructing a 'AttributeTypeListSetup'
-- in the General monad.
--
-- Throws an exception with an appropriate msg if one of the "requested"
-- attributes does not exist in the 'ObjectType'.
-------------------------------------------------------------------------------
toMkResult2GeneralMonad :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                        -> Either
                           (Any (AttributeType atConf dbTable))
                           a
                        -> GeneralResult a
toMkResult2GeneralMonad _ (Right x) = return x
toMkResult2GeneralMonad ot (Left (Any at)) =
  Left $ GeneralObjectModelError msg
  where
    msg = "AttributeTypeListSetup: " ++
          otCrossRefKey ot ++
          "/" ++ atCrossRefKey at

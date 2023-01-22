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
toMkResult2GeneralMonad _ (Right x) = pure x
toMkResult2GeneralMonad ot (Left (Any at)) =
  Left $ GeneralObjectModelError msg
  where
    msg = "AttributeTypeListSetup: " ++
          otCrossRefKey ot ++
          "/" ++ atCrossRefKey at

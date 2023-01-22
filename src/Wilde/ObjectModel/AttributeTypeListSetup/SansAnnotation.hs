-------------------------------------------------------------------------------
-- | Functionality for extraction of the 'Attribute's of an
-- 'Object' in a custom order.
--
-- Import qualified.
-------------------------------------------------------------------------------
module Wilde.ObjectModel.AttributeTypeListSetup.SansAnnotation
       (
         -- * Type

         Setup,
         getOt,
         getAts,

         -- * Constructors

         mk,
         mkGeneral,
         mkExclude,
         mkExcludeFromAll,

         -- * Application

         apply,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.Array.IArray as Array

import Wilde.ObjectModel.ObjectModel
import Wilde.ObjectModel.ObjectModelUtils (anyValueApply)

import Wilde.ObjectModel.AttributeTypeListSetup.Common


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | A sequence of 'AttributeType's that can be fetched from 'Object's
-- of a given 'ObjectType'.
--
-- A value of this type can only be constructed by functions in this
-- module, which garranties that all listed 'AttributeType's
-- indeed exist in the 'ObjectType', and thus also in that type's 'Object's.
-------------------------------------------------------------------------------
data Setup otConf atConf dbTable otNative idAtExisting idAtCreate =
   Setup
   {
     atlsOt       :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
   , atlsAts      :: [Any (AttributeType atConf dbTable)]
   , atlsGetAttrs :: Object otConf atConf dbTable otNative idAtExisting idAtCreate
                     -> [Any (Attribute atConf dbTable)]
   }

-------------------------------------------------------------------------------
-- | Gives a getter function for a sequence of 'Attribute's.
--
-- If a Left, then the given 'AttributeType' is not part of the
-- 'ObjectType'.
-------------------------------------------------------------------------------
mk :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
   -> [Any (AttributeType atConf dbTable)]
   -> Either (Any (AttributeType atConf dbTable))
      (Setup otConf atConf dbTable otNative idAtExisting idAtCreate)
mk ot ats =
  do
    validIndexes <- mapM indexOfAt ats
    return $ Setup
        {
        atlsOt       = ot,
        atlsAts      = ats,
        atlsGetAttrs = getAttrsFromObject validIndexes
        }
  where
    -- Each attribute paired with it's index in the array.
    atKeyIndexes = zip (map atCrossRefKey_anyValue $ otAttributeTypes ot) [0..]

    -- Any (AttributeType atConf dbTable) -> Either (Any (Attribute atConf dbTable)) Int
    indexOfAt atav@(Any (at@AttributeType { atCrossRefKey = atKey })) =
      maybe
      (Left atav)
      (Right)
      (lookup atKey atKeyIndexes)

    -- The method that is returned - looks up using indexes and the array.
    getAttrsFromObject :: [Int]
                       -> Object otConf atConf dbTable otNative idAtExisting idAtCreate
                       -> [Any (Attribute atConf dbTable)]
    getAttrsFromObject validIndexes o = map ((Array.!) attrArray) validIndexes
      where
        attrArray = oAttributeArray o


-------------------------------------------------------------------------------
-- | A setup of the 'AttributeTypes' of the 'ObjectType' in the default order,
-- except a list of excluded types.
-------------------------------------------------------------------------------
mkExcludeFromAll :: ObjectType           otConf atConf dbTable otNative idAtExisting idAtCreate
                 -> [Any (AttributeType atConf dbTable)]
                 -> GeneralResult (Setup otConf atConf dbTable otNative idAtExisting idAtCreate)
mkExcludeFromAll ot atsToExclude =
  mkExclude ot (otAttributeTypes ot) atsToExclude

-------------------------------------------------------------------------------
-- | A setup of the 'AttributeTypes' of the 'ObjectType' in the default order,
-- except a list of excluded types.
-------------------------------------------------------------------------------
mkExclude :: ObjectType           otConf atConf dbTable otNative idAtExisting idAtCreate
          -> [Any (AttributeType atConf dbTable)]
          -> [Any (AttributeType atConf dbTable)]
          -> GeneralResult (Setup otConf atConf dbTable otNative idAtExisting idAtCreate)
mkExclude ot atsToIncludeBeforeExclude atsToExclude =
   mkGeneral ot atsToInclude
  where
    atsToInclude               = filter filterPred $ atsToIncludeBeforeExclude
    filterPred (Any at) = not $ atCrossRefKey at `elem` crossRefKeysOfAtsToExclude
    crossRefKeysOfAtsToExclude = map (anyValueApply atCrossRefKey) atsToExclude


-------------------------------------------------------------------------------
-- | Constructs a 'AttributeTypeListSetup' in the General monad.
--
-- Throws an exception with an appropriate msg if one of the "requested"
-- attributes does not exist in the 'ObjectType'.
-------------------------------------------------------------------------------
mkGeneral :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
          -> [Any (AttributeType atConf dbTable)]
          -> GeneralResult (Setup otConf atConf dbTable otNative idAtExisting idAtCreate)
mkGeneral ot = toMkResult2GeneralMonad ot . mk ot

apply :: Setup otConf atConf dbTable otNative idAtExisting idAtCreate
      -> Object otConf atConf dbTable otNative idAtExisting idAtCreate
      -> [Any (Attribute atConf dbTable)]
apply = atlsGetAttrs

getOt :: Setup      otConf atConf dbTable otNative idAtExisting idAtCreate
      -> ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
getOt = atlsOt

getAts :: Setup otConf atConf dbTable otNative idAtExisting idAtCreate
      -> [Any (AttributeType atConf dbTable)]
getAts = atlsAts

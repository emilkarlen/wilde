-------------------------------------------------------------------------------
-- | Functionality for extraction of the 'Attribute's of an
-- 'Object' in a custom order.
--
-- Each 'Attribute' is paired with an \"annotation\".
--
-- Import qualified (clashes with Prelude).
-------------------------------------------------------------------------------
module Wilde.ObjectModel.AttributeTypeListSetup.WithAnnotation
       (
         Setup,
         mk,
         mkGeneral,
         apply,
         getOt,
         getAts,
         map,
         mapM,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Prelude as Prelude

import qualified Data.Array.IArray as Array

import Wilde.ObjectModel.ObjectModel

import Wilde.ObjectModel.AttributeTypeListSetup.Common


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data Setup otConf atConf dbTable otNative idAtExisting idAtCreate a =
   Setup
   {
     atlsaOt       :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
   , atlsaAts      :: [(Prelude.Int,Any (AttributeType atConf dbTable),a)]
   }

getOt :: Setup      otConf atConf dbTable otNative idAtExisting idAtCreate a
      -> ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
getOt = atlsaOt

getAts :: Setup otConf atConf dbTable otNative idAtExisting idAtCreate a
      -> [(Prelude.Int,Any (AttributeType atConf dbTable),a)]
getAts = atlsaAts


-- | Gives a getter function for a sequence of 'Attribute's.
--
-- If a Left, then the given 'AttributeType' is not part of the
-- 'ObjectType'.
mk :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
   -> [(Any (AttributeType atConf dbTable),a)]
   -> Prelude.Either
      (Any (AttributeType atConf dbTable))
      (Setup
       otConf atConf dbTable otNative idAtExisting idAtCreate a)
mk ot atAndAnnList =
  do
    idxAtAnnList <- Prelude.mapM indexOfAt atAndAnnList
    Prelude.pure Prelude.$ Setup
             {
               atlsaOt       = ot
             , atlsaAts      = idxAtAnnList
             }
  where

    -- Any (AttributeType atConf dbTable) -> Either (Any (Attribute atConf dbTable)) Int
    indexOfAt (at@(Any (AttributeType { atCrossRefKey = atKey })),atConf) =
      Prelude.maybe
      (Prelude.Left at)
      (\idx -> Prelude.Right (idx,at,atConf))
      (Prelude.lookup atKey atKeyIndexes)

    -- Each attribute paired with it's index in the array.
    atKeyIndexes = Prelude.zip
                   (Prelude.map atCrossRefKey_anyValue Prelude.$ otAttributeTypes ot)
                   [0..]

-------------------------------------------------------------------------------
-- | Constructs a 'AttributeTypeListSetup' in the General monad.
--
-- Throws an exception with an appropriate msg if one of the "requested"
-- attributes does not exist in the 'ObjectType'.
-------------------------------------------------------------------------------
mkGeneral :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
          -> [(Any (AttributeType atConf dbTable),a)]
          -> GeneralResult (Setup otConf atConf dbTable otNative idAtExisting idAtCreate a)
mkGeneral ot = toMkResult2GeneralMonad ot Prelude.. mk ot

apply :: Setup  otConf atConf dbTable otNative idAtExisting idAtCreate a
      -> Object otConf atConf dbTable otNative idAtExisting idAtCreate
      -> [(Any (Attribute atConf dbTable),a)]
apply
  (Setup { atlsaAts = theIdxAtAnn })
  (Object { oAttributeArray = theAttrArray})
  =
    Prelude.map getAttrAndAnn theIdxAtAnn
  where
    getAttrAndAnn (idx,_,annotation) = ((Array.!) theAttrArray idx,annotation)


-------------------------------------------------------------------------------
-- | Transforms a Setup in the fashion of map.
-------------------------------------------------------------------------------
map :: ((Any (AttributeType atConf dbTable),a) -> b)
    -> Setup otConf atConf dbTable otNative idAtExisting idAtCreate a
    -> Setup otConf atConf dbTable otNative idAtExisting idAtCreate b
map
  f
  setup@(Setup {
            atlsaOt       = theOt,
            atlsaAts      = theAts
            })
  =
   Setup
   {
     atlsaOt       = theOt
   , atlsaAts      = Prelude.map
                     (\(idx,at,a) -> (idx,at,f (at,a)))
                     theAts
   }

-------------------------------------------------------------------------------
-- | Transforms a Setup in the fashion of mapM.
-------------------------------------------------------------------------------
mapM :: Prelude.Monad m
     => ((Any (AttributeType atConf dbTable),a) -> m b)
     -> Setup    otConf atConf dbTable otNative idAtExisting idAtCreate a
     -> m (Setup otConf atConf dbTable otNative idAtExisting idAtCreate b)
mapM
  f
  setup@(Setup {
            atlsaOt       = theOt,
            atlsaAts      = theAts
            })
  =
    do
      newAts <- Prelude.mapM
                (
                  \(idx,at,a) ->
                    do
                      b <- f (at,a)
                      Prelude.pure (idx,at,b)
                )
                theAts
      Prelude.pure Prelude.$
        Setup
        {
          atlsaOt       = theOt
        , atlsaAts      = newAts
        }

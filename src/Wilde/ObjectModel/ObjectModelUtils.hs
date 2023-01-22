{-# LANGUAGE Rank2Types #-}

-- | Utilities related to the methods and data structures in 'ObjectModel'.
--
-- These utilities are put in a separate module to keep 'ObjectModel' as clean
-- as possible (so that it does not become to complex/difficult to understand).

module Wilde.ObjectModel.ObjectModelUtils
       (

         module Wilde.ObjectModel.ObjectModel,

         -- * anyValueApply:ers

         anyValueApply,
         anyValueApply2,
         anyOApply2_maybe,
         anyValueApplyM,

         anyOFilter,
         anyOApply,
         anyOApply2,
         anyOApplyM,
         toAnyForOtAndArg,

         castToAttributeType,

         lookupObjectTypeAny,
         lookupAttributeValue,
         anyValueFilter,

         -- * AttributeType configurations

         atTransformConfiguration,
         atSetConfiguration,
         otTransformConfiguration,
         otSetAtConfiguration,

         -- * Misc

         tupleString,


         mapAttributeTypeAnyValue,
         mapMAttributeTypeAnyValue,
         mapAttributeAnyValue,

         -- * Helper methods for translating general 'Object's.

         numAttributesError,
         numAttributesError2,

         doOtnUnhideAttribute,
         doOtnUnhide,
         )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------



import Data.List

import Wilde.Utils.Utils

import Wilde.ObjectModel.ObjectModel


-------------------------------------------------------------------------------
-- - ObjectTypeAny -
-------------------------------------------------------------------------------


lookupObjectTypeAny :: [AnyO (ObjectType otConf atConf)] -> CrossRefIdentifier -> Maybe (AnyO (ObjectType otConf atConf))
lookupObjectTypeAny objectTypes id =
  let
    isMatch ot = id == anyOApply otCrossRefKey ot
  in
   find isMatch objectTypes

lookupAttributeValue :: AttributeType atConf dbTable typeForExisting typeForCreate
                     -> Object otConf atConf dbTable otNative idAtExisting idAtForCreate
                     -> Maybe (ConvertResult typeForExisting)
lookupAttributeValue at o =
  fmap
  (castToAttributeType at)
  (find isAt (oAllAttributesAnyValue o))
  where
    atId = atCrossRefKey at
    isAt (Any a) = atId == atCrossRefKey (attrType a)

-- | Casts the value of an 'Attribute' to the type of an 'AttributeType'.
castToAttributeType :: AttributeType atConf dbTable e c
                    -> Any (Attribute atConf dbTable)
                    -> ConvertResult e
castToAttributeType targetAt@(AttributeType {}) (Any attr@(Attribute {})) =
  maybe
  (convError errMsg value)
  Right
  (cast value)
  where
    value = attrValue attr
    errMsg = "Target AttributeType = " ++ atCrossRefKey targetAt ++
             ", Source AttributeType = " ++ atCrossRefKey (attrType attr)

-------------------------------------------------------------------------------
-- - AttributeType configuration -
-------------------------------------------------------------------------------


atTransformConfiguration :: (atConf1 dbTable e c -> atConf2 dbTable e c)
                         -> AttributeType atConf1 dbTable e c
                         -> AttributeType atConf2 dbTable e c
atTransformConfiguration f at = at { atConfiguration = atConf2 }
  where
    atConf2 = f (atConfiguration at)

atSetConfiguration :: atConf2 dbTable e c
                   -> AttributeType atConf1 dbTable e c
                   -> AttributeType atConf2 dbTable e c
atSetConfiguration atConf2 at = at { atConfiguration = atConf2 }

otTransformConfiguration :: (forall e c . atConf1 dbTable e c -> atConf2 dbTable e c)
                         -> ObjectType otConf atConf1 dbTable otNative e c
                         -> ObjectType otConf atConf2 dbTable otNative e c
otTransformConfiguration f ot =
  ot
    {
      otIdAttributeType     = idAt
    , otNonIdAttributeTypes = nonIdAts
    }
  where
    idAt     = atTransformConfiguration f (otIdAttributeType ot)
    nonIdAts = map (anyValueApply2 (atTransformConfiguration f)) (otNonIdAttributeTypes ot)

otSetAtConfiguration :: (forall e c . atConf2 dbTable e c)
                     -> ObjectType otConf atConf1 dbTable otNative e c
                     -> ObjectType otConf atConf2 dbTable otNative e c
otSetAtConfiguration atConf2 ot =
  ot
    {
      otIdAttributeType     = idAt
    , otNonIdAttributeTypes = nonIdAts
    }
  where
    idAt     = atSetConfiguration atConf2 (otIdAttributeType ot)
    nonIdAts = map (anyValueApply2 (atSetConfiguration atConf2)) (otNonIdAttributeTypes ot)


-------------------------------------------------------------------------------
-- - Any -
-------------------------------------------------------------------------------


anyValueFilter :: (forall e c . t e c -> Bool)
                 -> [Any t]
                 -> [Any t]
anyValueFilter p [] = []
anyValueFilter p (x@(Any v) : xs) | p v       = x : anyValueFilter p xs
                                         | otherwise =     anyValueFilter p xs

-- | Applies a function that takes a \"plain\" value as argument to
-- a value wrapped in a 'Any'.
anyValueApply :: (forall e c . t e c -> a)
              -> Any t
                 -> a
anyValueApply f (Any x) = f x

anyValueApply2 :: (forall e c . t e c -> u e c)
               -> Any t
               -> Any u
anyValueApply2 f (Any x) = Any (f x)

-- | Applies a \"monadic\" function that takes a \"plain\" value as argument to
-- a value wrapped in a 'Any'.
anyValueApplyM :: Monad m
               => (forall e c . t e c -> m (u e c))
               -> Any t
               -> m (Any u)
anyValueApplyM f (Any x) =
  do
    x' <- f x
    return $ Any x'


-------------------------------------------------------------------------------
-- - AnyO -
-------------------------------------------------------------------------------


anyOFilter :: (forall dbTable otNative idAtE idAtC . t dbTable otNative idAtE idAtC -> Bool)
           -> [AnyO t]
           -> [AnyO t]
anyOFilter p [] = []
anyOFilter p (x@(AnyO v) : xs) | p v       = x : anyOFilter p xs
                               | otherwise =     anyOFilter p xs

-- | Applies a function that takes a \"plain\" value as argument to
-- a value wrapped in a 'AnyO'.
anyOApply :: (forall dbTable otNative idAtE idAtC . t dbTable otNative idAtE idAtC -> a)
          -> AnyO t
          -> a
anyOApply f (AnyO x) = f x

anyOApply2 :: (forall dbTable otNative idAtE idAtC .
               t dbTable otNative idAtE idAtC -> u dbTable otNative idAtE idAtC)
           -> AnyO t
           -> AnyO u
anyOApply2 f (AnyO x) = AnyO (f x)

anyOApply2_maybe :: (forall dbTable otNative idAtE idAtC .
                     t dbTable otNative idAtE idAtC -> Maybe (u dbTable otNative idAtE idAtC))
                 -> AnyO t
                 -> Maybe (AnyO u)
anyOApply2_maybe f (AnyO x) = maybe Nothing (Just . AnyO) (f x)

-- | Applies a \"monadic\" function that takes a \"plain\" value as argument to
-- a value wrapped in a 'AnyO'.
anyOApplyM :: Monad m
           => (forall dbTable otNative idAtE idAtC .
               t dbTable otNative idAtE idAtC -> m (u dbTable otNative idAtE idAtC))
           -> AnyO t
           -> m (AnyO u)
anyOApplyM f (AnyO x) =
  do
    x' <- f x
    return $ AnyO x'


-- | Helper for transforming a common kind of functions that operate on
-- 'ObjectType'-like objects to an equivalent function operating on the same
-- things wrapped in 'AnyO'.
toAnyForOtAndArg :: (Functor m,Functor r)
                 => (forall dbTable otNative idAtE idAtC .
                     t dbTable otNative idAtE idAtC
                     -> a
                     -> m (r (u dbTable otNative idAtE idAtC)))
                 -> (a,AnyO t)
                 -> m (r (AnyO u))
toAnyForOtAndArg f (a,AnyO t) = fmap (fmap AnyO) $ f t a


-------------------------------------------------------------------------------
-- - tuple string -
-------------------------------------------------------------------------------


-- | Produces a String, formated as a tuple value, where each 'Attribute'
-- is a "component".
tupleString :: Object otConf atConf dbTable otNative idAtExisting idAtCreate -> String
tupleString object =
  concat $ ["("] ++ intersperse "," stringValues ++ [")"]
  where
    stringValues = mapAttributeAnyValue showAttrValue $ oAllAttributesAnyValue object

showAttrValue :: Attribute atConf dbTable e c -> String
showAttrValue (Attribute { attrValue = v, attrType = (AttributeType {}) }) = show v


-------------------------------------------------------------------------------
-- | Convenience method for map on types using Existential Quantification -
-------------------------------------------------------------------------------


mapAttributeTypeAnyValue :: (forall typeForExisting typeForCreate .
                             AttributeType atConf dbTable typeForExisting typeForCreate -> a)
                         -> [Any (AttributeType atConf dbTable)]
                         -> [a]
mapAttributeTypeAnyValue f = map $ \(Any at) -> f at

mapMAttributeTypeAnyValue :: Monad m
                          => (forall typeForExisting typeForCreate .
                              AttributeType atConf dbTable typeForExisting typeForCreate -> m a)
                          -> [Any (AttributeType atConf dbTable)]
                          -> m [a]
mapMAttributeTypeAnyValue f = mapM $ \(Any at) -> f at

mapAttributeAny :: (forall atConf dbTable typeForExisting typeForCreate .
                    (Typeable typeForExisting,Show typeForExisting)
                => Attribute atConf dbTable typeForExisting typeForCreate -> a)
                -> [AttributeAny]
                -> [a]
mapAttributeAny f = map $ \(AttributeAny at) -> f at

mapAttributeAnyValue :: (forall typeForExisting typeForCreate .
                         Attribute atConf dbTable typeForExisting typeForCreate -> a)
                     -> [Any (Attribute atConf dbTable)]
                     -> [a]
mapAttributeAnyValue f = map $ \(Any at) -> f at


-------------------------------------------------------------------------------
-- - error helpers -
-------------------------------------------------------------------------------


-- | Error in the number of attributes, when translating an 'Object' to it's
-- "native" form ('otToNative').
numAttributesError :: Int -- ^ actual
                   -> Int -- ^ expected
                   -> ObjectToNativeError
numAttributesError actual' expected' = ObjectToNativeError "" $ NumAttributesError mismatch
  where
    mismatch = Mismatch
               {
                 actual   = actual',
                 expected = expected'
               }

numAttributesError2 :: [attr] -- ^ actual
                    -> Int -- ^ expected
                    -> Either ObjectToNativeError a
numAttributesError2 attrs expected' =
  Left $ numAttributesError (length attrs) expected'


-------------------------------------------------------------------------------
-- - "unhiding" of attribute values -
-------------------------------------------------------------------------------


doOtnUnhideAttribute :: (Typeable b)
                     => Attribute atConf dbTable typeForExisting typeForCreate
                     -> Either ObjectToNativeError b
doOtnUnhideAttribute (Attribute at@(AttributeType {}) val _) =
  either (Left . (ObjectToNativeError msg . AttributeTypeError)) Right $ doUnhide val
  where
    msg = atCrossRefKey at

doOtnUnhide :: (Typeable a,Typeable b)
            => a -> Either ObjectToNativeError b
doOtnUnhide = either (Left . (anonOTN . AttributeTypeError)) Right . doUnhide

doUnhide :: (Typeable a,Typeable b)
         => a -> Either (Mismatch TypeRep) b
doUnhide a = doUnhide' undefined a

doUnhide' :: (Typeable a,Typeable b)
          => b -- ^ Needed just for typing. Never accessed (so undefined is a fine value).
          -> a
          -> Either (Mismatch TypeRep) b
doUnhide' bForType a =
  case cast a of
    Nothing -> Left $ Mismatch { expected = typeOf bForType, actual = typeOf a}
    Just b  -> Right b

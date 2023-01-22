{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

-- | Defintions for an 'ObjectType' configuration that supports database IO
-- and has DDL info for the 'AttributeType's.
module Wilde.ApplicationConstruction.ObjectTypeConfiguration.ObjectTypeWithAtDdlInformation
       (
         ObjectTypeWithDdlInfo,
         AnyO(..),
         anyODdlApply,
         anyODdlApply2,
         anyODdlApplyM,
         )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.ObjectModel.ObjectModel as ObjectModel

import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.DdlAtAnnotation as DdlAtAnnotation
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.Database as OtDatabaseConfig


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------



type ObjectTypeWithDdlInfo = ObjectModel.ObjectType
                             OtDatabaseConfig.Configuration
                             DdlAtAnnotation.Configuration

-- | A variant of 'Any' for 'ObjectType's and 'Object's.
data AnyO t = forall dbTable otNative idAtExisting idAtCreate .
              AnyO (t
                    OtDatabaseConfig.Configuration
                    DdlAtAnnotation.Configuration
                    dbTable
                    otNative
                    idAtExisting
                    idAtCreate)

-- | Applies a function that takes a \"plain\" value as argument to
-- a value wrapped in a 'AnyO'.
anyODdlApply :: (forall dbTable otNative idAtE idAtC .
              t OtDatabaseConfig.Configuration DdlAtAnnotation.Configuration dbTable otNative idAtE idAtC -> a)
             -> AnyO t
             -> a
anyODdlApply f (AnyO x) = f x

anyODdlApply2 :: (forall dbTable otNative idAtE idAtC .
                  t OtDatabaseConfig.Configuration DdlAtAnnotation.Configuration dbTable otNative idAtE idAtC
                  -> u OtDatabaseConfig.Configuration DdlAtAnnotation.Configuration dbTable otNative idAtE idAtC)
              -> AnyO t
              -> AnyO u
anyODdlApply2 f (AnyO x) = AnyO (f x)

-- | Applies a \"monadic\" function that takes a \"plain\" value as argument to
-- a value wrapped in a 'AnyO'.
anyODdlApplyM :: Monad m =>
              (forall otConf atConf dbTable otNative idAtE idAtC .
               t otConf atConf dbTable otNative idAtE idAtC
               -> m (u otConf atConf dbTable otNative idAtE idAtC))
              -> AnyO t
              -> m (AnyO u)
anyODdlApplyM f (AnyO x) =
  do
    x' <- f x
    pure $ AnyO x'

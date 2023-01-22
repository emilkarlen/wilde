{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-------------------------------------------------------------------------------
-- | Type for an annotation of AttributeType that provides
-- information for generating database DDL.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.AttributeTypeConfiguration.DdlAtAnnotation
       (
         module Database,

         Configuration,
         DdlAtAnnotation,

         mkDdlAtAnnotation,

         transformDdlAtAnnotation,
         transformDdlAtAnnotationColumnInfos,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.List.NonEmpty as NonEmpty

import Wilde.Database.SqlDdlInfo ( DdlColumnInfo )

import Wilde.ObjectModel.ObjectModel as ObjectModel hiding (AnyO)
import Wilde.ObjectModel.Database as Database

import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.UiIoAndDbIo as UiIoAndDbIo


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Empty annotation type used together with 'AnyO'.
newtype DdlAtAnnotation dbTable e c =
  DdlAtAnnotation
  {
    ddlAtaGetColumns :: NonEmpty.NonEmpty (DdlColumnInfo dbTable)
  }

-- | Constructor of 'DdlAtAnnotation'.
mkDdlAtAnnotation :: NonEmpty.NonEmpty (DdlColumnInfo dbTable)
                  -> DdlAtAnnotation dbTable e c
mkDdlAtAnnotation = DdlAtAnnotation

-------------------------------------------------------------------------------
-- | Configuration with DDL information
-------------------------------------------------------------------------------
--type Configuration dbTable e c = UiIoAndDbIo.Configuration DdlAtAnnotation dbTable e c
type Configuration = UiIoAndDbIo.Configuration DdlAtAnnotation

-- | Transforms the list of 'DdlColumnInfo's of a
-- 'DdlAtAnnotation'.
transformDdlAtAnnotation :: (NonEmpty.NonEmpty (DdlColumnInfo dbTableA)
                         -> NonEmpty.NonEmpty (DdlColumnInfo dbTableB))
                         -> DdlAtAnnotation dbTableA e c
                         -> DdlAtAnnotation dbTableB e c
transformDdlAtAnnotation f (DdlAtAnnotation xs) = DdlAtAnnotation $ f xs

-- | Transforms each of 'DdlColumnInfo' of a
-- 'DdlAtAnnotation' in the same way.
transformDdlAtAnnotationColumnInfos :: (DdlColumnInfo dbTable -> DdlColumnInfo dbTable)
                                    -> DdlAtAnnotation dbTable e c
                                    -> DdlAtAnnotation dbTable e c
transformDdlAtAnnotationColumnInfos f (DdlAtAnnotation xs) = DdlAtAnnotation $ fmap f xs


-------------------------------------------------------------------------------
-- - Any... -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - instances -
-------------------------------------------------------------------------------


instance Database.DDL (UiIoAndDbIo.Configuration DdlAtAnnotation) where
  atDdlInfo = ddlAtaGetColumns .
              UiIoAndDbIo.annotation .
              ObjectModel.atConfiguration

instance Database.DATABASE_IO_AND_DDL (UiIoAndDbIo.Configuration DdlAtAnnotation) where

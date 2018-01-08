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


import qualified Wilde.Utils.NonEmptyList as NonEmpty

import Wilde.Database.SqlDdlInfo

import Wilde.ObjectModel.ObjectModel as ObjectModel hiding (AnyO)
import Wilde.ObjectModel.Database as Database

import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.UiIoAndDbIo as UiIoAndDbIo


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Empty annotation type used together with 'AnyO'.
data DdlAtAnnotation dbTable e c =
  DdlAtAnnotation
  {
    ddlAtaGetColumns :: NonEmpty.List (DdlColumnInfo dbTable)
  }

-- | Constructor of 'DdlAtAnnotation'.
mkDdlAtAnnotation :: NonEmpty.List (DdlColumnInfo dbTable)
                  -> DdlAtAnnotation dbTable e c
mkDdlAtAnnotation = DdlAtAnnotation

-------------------------------------------------------------------------------
-- | Configuration with DDL information
-------------------------------------------------------------------------------
--type Configuration dbTable e c = UiIoAndDbIo.Configuration DdlAtAnnotation dbTable e c
type Configuration = UiIoAndDbIo.Configuration DdlAtAnnotation

-- | Transforms the list of 'DdlColumnInfo's of a
-- 'DdlAtAnnotation'.
transformDdlAtAnnotation :: (NonEmpty.List (DdlColumnInfo dbTableA)
                         -> NonEmpty.List (DdlColumnInfo dbTableB))
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

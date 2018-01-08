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
-- | Functionallity for ordering the CREATE-TABLE statements of the DDL
-- for avoiding referencing tables (via foreign keys) that have not yet
-- been created.
-------------------------------------------------------------------------------
module Wilde.ApplicationTool.Commands.OrderCreateTables
       (
         SCC(..),
         orderObjectTypesSoThatReferencesAreCreatedAfterTargets,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------

import Data.Maybe

import qualified Data.Set as Set
import Data.Graph

import qualified Wilde.Utils.NonEmptyList as NonEmpty

import Wilde.Database.Sql
import Wilde.Database.SqlDdlInfo

import Wilde.Media.Database

import Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.Database as Database
import Wilde.ObjectModel.ObjectModelUtils
import qualified Wilde.ObjectModel.Database as DatabaseClasses (DDL(..))

import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.DdlAtAnnotation as DdlAtAnnotation
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.ObjectTypeWithAtDdlInformation as ObjectTypeWithAtDdlInformation


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


orderObjectTypesSoThatReferencesAreCreatedAfterTargets :: [ObjectTypeWithAtDdlInformation.AnyO ObjectType]
                                                       -> [SCC (ObjectTypeWithAtDdlInformation.AnyO ObjectType)]
orderObjectTypesSoThatReferencesAreCreatedAfterTargets =
  stronglyConnComp . map mkNodeAndDependencies

mkNodeAndDependencies :: ObjectTypeWithAtDdlInformation.AnyO ObjectType -> NodeAndDependencies
mkNodeAndDependencies aot@(ObjectTypeWithAtDdlInformation.AnyO ot@(ObjectType {})) =
  (aot,name,referencedTables)
  where
    name :: TableName
    name = tableName . Database.otDatabaseTable $ ot
    referencedTables = Set.toList . Set.fromList $ getColReferences ot
    getColReferences :: (Show e, Typeable e)
                        => ObjectTypeWithAtDdlInformation.ObjectTypeWithDdlInfo dbTable otNative e c
                        -> [TableName]
    getColReferences = map mkForeignKeyTarget .
                       concatMap (anyValueApply getForeignKeyTarget) .
                       otAttributeTypes

getForeignKeyTarget :: AttributeType DdlAtAnnotation.Configuration dbTable e c 
                    -> [ForeignKeyTarget]
getForeignKeyTarget = catMaybes .
                      map foreignKey .
                      NonEmpty.toList .
                      DatabaseClasses.atDdlInfo

mkForeignKeyTarget :: ForeignKeyTarget -> TableName
mkForeignKeyTarget = fkTargetTable

type TableName           = SqlIdentifier
type NodeAndDependencies = (ObjectTypeWithAtDdlInformation.AnyO ObjectType,TableName,[TableName])

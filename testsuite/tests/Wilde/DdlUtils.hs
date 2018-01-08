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

module Wilde.DdlUtils
       (
         TableName
         
       , CreateTablePredicate
       , AlterTablePredicate

       , isCreateTableWithName         
         
       , isAlterTableWithName

       , isCreateTableAnd
       , isAlterTableAnd
         
       , createTableHasName
       , alterTableHasName
         
       ,andAll
       )
       where


import Wilde.Database.BackEndDdl

type CreateTablePredicate columnInfo = BackEndTableInfo columnInfo -> Bool

type AlterTablePredicate columnInfo = (SqlIdentifier,[AlterSpecification columnInfo]) -> Bool


type TableName = SqlIdentifier

isCreateTableWithName :: SqlIdentifier -> DdlStatement a -> Bool
isCreateTableWithName expectedTableName =
  isCreateTableAnd [\tbl -> tblName tbl == expectedTableName]

isAlterTableWithName :: SqlIdentifier -> DdlStatement a -> Bool
isAlterTableWithName expectedTableName =
  isAlterTableAnd [\(tblName,_) -> tblName == expectedTableName]

isCreateTableAnd :: [BackEndTableInfo columnInfo -> Bool]
                    -> DdlStatement columnInfo 
                    -> Bool
isCreateTableAnd ps (AlterTable _)  = False
isCreateTableAnd ps (CreateTable x) = andAll ps x

isAlterTableAnd :: [(SqlIdentifier,[AlterSpecification columnInfo]) -> Bool]
                    -> DdlStatement columnInfo 
                    -> Bool
isAlterTableAnd ps (CreateTable _)  = False
isAlterTableAnd ps (AlterTable x) = andAll ps x

andAll :: [a -> Bool] -> a -> Bool
andAll ps x = and $ map (\p -> p x) ps

createTableHasName :: SqlIdentifier -> CreateTablePredicate a
createTableHasName name tbl = tblName tbl == name

alterTableHasName :: SqlIdentifier -> AlterTablePredicate a
alterTableHasName name (tblName,_) = tblName == name

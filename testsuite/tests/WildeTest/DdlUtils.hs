module WildeTest.DdlUtils
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

module WildeTest.ApplicationTool.ApplicationToolTest
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import Wilde.Database.BackEndDdl

import Wilde.ObjectModel.ObjectModel

import qualified Wilde.ApplicationConstruction.StandardServices as SS
import Wilde.ApplicationTool.Commands.SqlDdl

import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.ObjectTypeWithAtDdlInformation as ObjectTypeWithAtDdlInformation

import TestResources.ReferenceObjectType
    ( ObjectTypeSetup, PkAndRefO, mkPkAndRefOts )
import qualified ObjectModel

import WildeTest.DdlUtils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | All tests in this module.
theTest =
  TestList
  [
    "Create Referring tables after the reference target tables" ~:

    assert ( (isCreateTableWithName refTableName) (getLast (backEndDdlStmtsOf aots)) )

  , "Create FOREIGN KEYs after CREATE TABLEs for circular dependencies" ~:

    circularTablesCheck (backEndDdlStmtsOf aotsCircular)

  ]
  where
    refTableName = getTableName refOt

    backEndDdlStmtsOf aots = [ commentedValue x | x <- mkDdlStatements aots]

    aots = map getOt ObjectModel.objectModel :: [ObjectTypeWithAtDdlInformation.AnyO ObjectType]

    refOt :: ObjectTypeWithAtDdlInformation.AnyO ObjectType
    refOt = getOt ObjectModel.aotsReference

    getOt :: ObjectTypeWithAtDdlInformation.AnyO SS.ObjectTypeSetup -> ObjectTypeWithAtDdlInformation.AnyO ObjectType
    getOt = ObjectTypeWithAtDdlInformation.anyODdlApply2 SS.objectType

getLast :: [a] -> a
getLast [] = error "getLast: empty list"
getLast xs = head $ reverse xs

getTableName :: ObjectTypeWithAtDdlInformation.AnyO ObjectType -> CrossRefIdentifier
getTableName = ObjectTypeWithAtDdlInformation.anyODdlApply otCrossRefKey

otsA_refB, otsB_refC, otsC_refB :: PkAndRefO ObjectTypeSetup

otsA_refB = mkPkAndRefOts "a" otsB_refC
otsB_refC = mkPkAndRefOts "b" otsC_refB
otsC_refB = mkPkAndRefOts "c" otsB_refC

aotsCircular :: [ObjectTypeWithAtDdlInformation.AnyO ObjectType]
aotsCircular = [ObjectTypeWithAtDdlInformation.AnyO (SS.objectType otsA_refB)
               ,ObjectTypeWithAtDdlInformation.AnyO (SS.objectType otsB_refC)
               ,ObjectTypeWithAtDdlInformation.AnyO (SS.objectType otsC_refB)
               ]

circularTablesCheck :: [DdlStatement a] -> Assertion
circularTablesCheck [tbl_inCircle1
                    ,tbl_inCircle2
                    ,fk_inCircle1
                    ,fk_inCircle2
                    ,tbl_a] =
  do
    b_or_C_first_and_has_no_fks
    fKs_for_B_or_C_after_TABLEs_for_B_and_C
    assert $ isCreateTableWithName "a" tbl_a
  where
    b_or_C_first_and_has_no_fks =
      assertBool "b_or_C_first_and_has_no_fks" $
      (isTablesWithNameAndNoFKs
       [("b",tbl_inCircle1)
       ,("c",tbl_inCircle2)]
      )
      ||
      (isTablesWithNameAndNoFKs
       [("c",tbl_inCircle1)
       ,("b",tbl_inCircle2)]
      )

    fKs_for_B_or_C_after_TABLEs_for_B_and_C =
      assertBool "fKs_for_B_or_C_after_TABLEs_for_B_and_C" $
      isAlterTableWithNameAndOneAddFk "b" fk_inCircle1 &&
      isAlterTableWithNameAndOneAddFk "c" fk_inCircle2
      ||
      isAlterTableWithNameAndOneAddFk "c" fk_inCircle1 &&
      isAlterTableWithNameAndOneAddFk "b" fk_inCircle2

    isAlterTableWithNameAndOneAddFk name ddl =
      isAlterTableAnd [alterTableHasName name
                      ,\(_,specList) -> length (specList) == 1
                      ] ddl

    isTablesWithNameAndNoFKs :: [(TableName,DdlStatement a)] -> Bool

    isTablesWithNameAndNoFKs tableName_and_ddl_list =
      and $ map isTableWithNameAndNoFKs tableName_and_ddl_list

    isTableWithNameAndNoFKs :: (TableName,DdlStatement a) -> Bool

    isTableWithNameAndNoFKs (name,ddl) =
      isCreateTableAnd [createTableHasName name
                       ,\tbl -> null (tblForeignKeys tbl)
                       ] ddl

circularTablesCheck xs = assertFailure $ "Expected number of DDL stmts: 5, found " ++ (show (length xs))

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

module WildeTest.ApplicationConstruction.AttributeTypesListFooterTest
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import TestResources.Testing.AssertUtils
import TestResources.PresentationMonadUtils

import Wilde.GenericUi.Value (AnyVALUE(..), valueString)

import Wilde.GenericUi.AbstractTable (Cell(..))
import Wilde.WildeUi.TableUtils (cellStd)

import Wilde.WildeUi.StdValueTypes (BoolValueAsCheckBox(..))

import Wilde.ObjectModel.ObjectModel
import Wilde.ObjectModel.Presentation

import qualified Wilde.ApplicationConstruction.UserInteraction.Output.ObjectListSetup as ObjListSetup (GetFooterRowsConstructor(..))

import Wilde.ApplicationConstruction.UserInteraction.Output.AttributeTypesListFooter

import TestResources.TestData

import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.DdlAtAnnotation as DdlAtAnnotation
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.Database as OtDbConfig


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | All tests in this module.
theTest =
  TestList
  [
    -- "Construction of footer rows" ~:
    
    -- [["2","", "3"] , ["F","",""]] ~=? footerRowsAsStrings
    
    "Construction of footer rows" ~:
    
    (TestCase $
     (emptyEnv `check`
      (failOnError $
       failOnNothing
       (\frc -> assertEqual
                "msg-prefix"
                (footerRowsAsStrings frc) 
                [["2","", "3"] , ["T","",""]]))
     )
     frcGetter
    )
    
  --   assert ( (isCreateTableWithName refTableName) (getLast (backEndDdlStmtsOf aots)) )
    
  -- , "Create FOREIGN KEYs after CREATE TABLEs for circular dependencies" ~:
    
  --   circularTablesCheck (backEndDdlStmtsOf aotsCircular)
             
  ]

frcGetter :: ObjListSetup.GetFooterRowsConstructor 
             (AttributeTypesFooterSpecification DdlAtAnnotation.Configuration PkNameTable) 
             OtDbConfig.Configuration
             DdlAtAnnotation.Configuration 
             PkNameTable 
             () 
             PrimaryKeyType 
             PrimaryKeyType
frcGetter = attributeTypesFooterCellsGetter otPkName atCellConstructors

atCellConstructors :: AttributeTypesFooterSpecification DdlAtAnnotation.Configuration PkNameTable
atCellConstructors = [Any pkAtFooters,Any nameAtFooters]
  
pkAtFooters :: AttributeTypeFooterSpecification 
               DdlAtAnnotation.Configuration 
               PkNameTable 
               PrimaryKeyType 
               PrimaryKeyType
pkAtFooters =
  AttributeTypeFooterSpecification
  {
    atfcAttributeType    = at_pk
  , atfcCellConstructors = [sumCell_show]
  }

nameAtFooters :: AttributeTypeFooterSpecification 
                 DdlAtAnnotation.Configuration 
                 PkNameTable 
                 String 
                 String
nameAtFooters =
  AttributeTypeFooterSpecification
  {
    atfcAttributeType    = at_name
  , atfcCellConstructors = [numObjectsCell
                           ,evenNumObjectsCell]
  }

columns :: [Maybe (Any (AttributeType DdlAtAnnotation.Configuration PkNameTable))]
columns = [Just $ Any at_name, Nothing, Just $ Any at_pk]

footerRowsAsStrings :: FooterRowsConstructor 
                       (AttributeTypesFooterSpecification otConf PkNameTable) 
                       OtDbConfig.Configuration 
                       DdlAtAnnotation.Configuration 
                       PkNameTable 
                       () 
                       PrimaryKeyType 
                       PrimaryKeyType
                    -> [[String]]
footerRowsAsStrings frc = cellsAsStrings footerRows
  where
  footerRows = footerRowsForObjects columns frc [oPkName1,oPkName2]


cellsAsStrings :: FooterRows -> [[String]]
cellsAsStrings (_,rows) = map (map getCellValueStringRepresentation) rows
                               
getCellValueStringRepresentation :: WildeStyledCell -> String
getCellValueStringRepresentation (Styling { sStyled = Cell { cellContent = AnyVALUE value } } ) = valueString value


-------------------------------------------------------------------------------
-- - Test footer cell constructors -
-------------------------------------------------------------------------------


-- Test footer cell constructor
evenNumObjectsCell :: AnyCellConstructor a
evenNumObjectsCell = AnyCellConstructor $ evenNumObjectsCell'

-- Tells if the number of objects is even or odd, by using an accumulator value (bool).
--
-- Using an accumulator value makes it possible to test the accumulation.
evenNumObjectsCell' :: CellConstructor Bool a
evenNumObjectsCell' =
  CellConstructor
  {
    fccInitial     = True
  , fccAccumulator = \(_,acc) -> not acc
  , fccMkCell      = \numObjects oddNumObjs -> [cellStd $ BoolValueAsCheckBox oddNumObjs]
  }

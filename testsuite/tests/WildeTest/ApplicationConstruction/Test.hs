{-
Copyright 2013 Emil Karlén.

This file is part of WildeTest.

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

module WildeTest.ApplicationConstruction.Test
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import qualified WildeTest.ApplicationConstruction.AttributeTypeTest
import qualified WildeTest.ApplicationConstruction.SqlExprParserTest
import qualified WildeTest.ApplicationConstruction.DateParserTest
import qualified WildeTest.ApplicationConstruction.AttributeTypesListFooterTest
import qualified WildeTest.ApplicationConstruction.StandardServices.WildeSqlInputerTest


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theTest = TestList
          [ WildeTest.ApplicationConstruction.AttributeTypeTest.theTest
          , WildeTest.ApplicationConstruction.SqlExprParserTest.theTest
          , WildeTest.ApplicationConstruction.DateParserTest.theTest
          , WildeTest.ApplicationConstruction.AttributeTypesListFooterTest.theTest
          , WildeTest.ApplicationConstruction.StandardServices.WildeSqlInputerTest.theTest
          ]

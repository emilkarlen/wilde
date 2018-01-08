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

module Wilde.ApplicationConstruction.Test
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import qualified Wilde.ApplicationConstruction.ObjectTypeTest
import qualified Wilde.ApplicationConstruction.SqlExprParserTest
import qualified Wilde.ApplicationConstruction.DateParserTest
import qualified Wilde.ApplicationConstruction.AttributeTypesListFooterTest
import qualified Wilde.ApplicationConstruction.StandardServices.WildeSqlInputerTest


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theTest = TestList
          [ Wilde.ApplicationConstruction.ObjectTypeTest.theTest
          , Wilde.ApplicationConstruction.SqlExprParserTest.theTest
          , Wilde.ApplicationConstruction.DateParserTest.theTest
          , Wilde.ApplicationConstruction.AttributeTypesListFooterTest.theTest
          , Wilde.ApplicationConstruction.StandardServices.WildeSqlInputerTest.theTest
          ]

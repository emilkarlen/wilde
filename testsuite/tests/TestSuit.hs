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

module TestSuit 
       (
         suit,
         runSuit,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Control.Monad

import Test.HUnit

import WildeTest.Utils.ListUtilsTest
import WildeTest.ObjectModel.Test
import WildeTest.Media.Test
import WildeTest.Render.Cgi.ElementSetIoTest
import WildeTest.ApplicationConstruction.Test
import WildeTest.ApplicationTool.ApplicationToolTest
import WildeTest.Driver.Tests


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


suit = TestList
       [ WildeTest.Utils.ListUtilsTest.theTest
       , WildeTest.ObjectModel.Test.theTest
       , WildeTest.Media.Test.theTest
       , WildeTest.Render.Cgi.ElementSetIoTest.theTest
       , WildeTest.ApplicationConstruction.Test.theTest
       , WildeTest.ApplicationTool.ApplicationToolTest.theTest
       , WildeTest.Driver.Tests.theTest
       ]

runSuit :: IO ()
runSuit = void $ runTestTT suit

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
       [ "list utils" ~:
         WildeTest.Utils.ListUtilsTest.theTest
       , "object model" ~:
         WildeTest.ObjectModel.Test.theTest
       , "media" ~:
         WildeTest.Media.Test.theTest
       , "element set" ~:
         WildeTest.Render.Cgi.ElementSetIoTest.theTest
       , "app con" ~:
         WildeTest.ApplicationConstruction.Test.theTest
       , "app tool" ~:
         WildeTest.ApplicationTool.ApplicationToolTest.theTest
       , "driver" ~:
         WildeTest.Driver.Tests.theTest
       ]

runSuit :: IO ()
runSuit = void $ runTestTT suit

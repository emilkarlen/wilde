{-# LANGUAGE OverloadedStrings #-}
module WildeTest.Driver.WaiServer.FilePathHandling.PathIsValid
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.List
import qualified Data.Text as T
import qualified Data.Map as M
import Test.HUnit

import           WildeTest.Driver.WaiServer.FilePathHandling.TestResources

import qualified Wilde.Driver.Application.WaiServer.RequestHandling.File.PathHandling as Sut


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theTest :: Test
theTest =
  TestLabel "pathIsValid" $
  TestList
    [
      "invalid" ~: pathIsValid_invalid
    , "valid"   ~: pathIsValid_valid
    ]

pathIsValid_invalid :: Test
pathIsValid_invalid = TestList
    [
      "empty" ~: tc []

    , "single empty component" ~: tc [""]

    , "1 empty, 1 non-empty component" ~:
      tcs ["", "non-empty.ext"]

    , "1 empty, 2 non-empty component" ~:
      tcs ["", "non-empty1.ext", "ne2.ext"]

    , "2 empty, 1 non-empty component" ~:
      tcs ["", "", "ne.ext"]

    , "missing extension" ~: TestList
      [
        "1 comp" ~:
        tc ["without-extension"]

      , "2 comps" ~:
        tc ["with.extension", "without-extension"]

      , "empty" ~:
        tc ["head."]
      ]

    , "relative dir components" ~: TestList
      [
        "just relative dir '.'" ~:
        tc ["."]
  
      , "relative dir component '.'" ~:
        tcs [".", "valid.ext"]
  
      , "just relative dir component '..'" ~:
        tc [".."]
  
      , "relative dir component '..'" ~:
        tcs ["..", "valid.ext"]
      ]

    , "leading dots does not separate extension" ~: TestList
      [
        "1 dot"  ~: tc [".hello"]
      , "2 dots" ~: tc ["..hello"]
      , "3 dots" ~: tc ["...hello"]
      ]
    ]
    where
        tc :: Sut.RequestPath -> Test
        tc actual = TestCase $ assertEqual "" False $ Sut.pathIsValid actual

        tcs :: Sut.RequestPath -> Test
        tcs components = TestList [
            TestLabel (show path) (tc path) | path <- permutations components
            ]


pathIsValid_valid :: Test
pathIsValid_valid = TestList
    [
      "1 comp" ~: TestList
        [
          "single ext"  ~: tc ["head.ext"]
        , "1 char ext"  ~: tc ["head.x"]
        , "leading dot" ~: tc [".head.ext"]
        , "multi ext"   ~: tc ["head.ext1.ext2"]
        ]
    , "2 comp (multi ext)"   ~: tc ["1st", "head.ext1.ext2"]
    , "wo ext, w ext"        ~: tc ["wo-ext", "w.ext"]
    , "w ext,  w ext"        ~: tc ["1w.ext", "2w.ext"]
    ]
    where
        tc :: Sut.RequestPath -> Test
        tc actual = TestCase $ assertEqual "" True $ Sut.pathIsValid actual

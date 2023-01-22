module WildeTest.Utils.ListUtilsTest
       (
         theTest
       )
       where

-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import Wilde.Utils.ListUtils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | All tests in this module.
theTest =
  TestList
  [ testSplitOn
  , testRenderCommaList
  ]


-------------------------------------------------------------------------------
-- - splitOn -
-------------------------------------------------------------------------------


testSplitOn :: Test
testSplitOn =
  TestList
  [ "splitOn (empty)" ~:

    [] ~=? (splitOn '.' "")

  , "splitOn (only separator 1)" ~:

    [[],[]] ~=? (splitOn '.' ".")

  , "splitOn (only separator 2)" ~:

    [[],[],[],[]] ~=? (splitOn '.' "...")

  , "splitOn (no separator 1)" ~:

    ["a"] ~=? (splitOn '.' "a")

  , "splitOn (no separator 2)" ~:

    ["ab"] ~=? (splitOn '.' "ab")

  , "splitOn (prefix separator)" ~:

    ["","a"] ~=? (splitOn '.' ".a")

  , "splitOn (sufix separator)" ~:

    ["a",""] ~=? (splitOn '.' "a.")

  , "splitOn (infix separator)" ~:

    ["a","b"] ~=? (splitOn '.' "a.b")

  , "splitOn (infix separator 2)" ~:

    ["abc","bcd"] ~=? (splitOn '.' "abc.bcd")

  , "splitOn (pre,in,suix separator)" ~:

    ["abc","bcd","","cde","",""] ~=? (splitOn '.' "abc.bcd..cde..")

  ]


-------------------------------------------------------------------------------
-- - renderCommaList -
-------------------------------------------------------------------------------


testRenderCommaList :: Test
testRenderCommaList =
  TestLabel "renderCommaList" $
  TestList
  [ "renderCommaList (empty)" ~:

    "" ~=? (renderCommaList [])

  , "renderCommaList (single empty)" ~:

    "" ~=? (renderCommaList [""])

  , "renderCommaList (many empty)" ~:

    "" ~=? (renderCommaList ["","",""])

  , "renderCommaList (single)" ~:

    "x" ~=? (renderCommaList ["x"])

  , "renderCommaList (mixed)" ~:

    "x,y,xyz,abc" ~=? (renderCommaList ["","x","y","","xyz","abc",""])
  ]

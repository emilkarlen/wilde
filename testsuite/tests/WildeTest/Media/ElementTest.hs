-- | Tests of the module 'UserInteractionMedia'.
module WildeTest.Media.ElementTest
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import TestResources.Testing.TestUtils
import qualified TestResources.TestData as TD

import Wilde.Media.Element


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | All tests in this module.
theTest :: Test
theTest = TestList
          [ elementKeyConstruction
          , elementKeyRenderingAndParsing
          , elementValueElementKeyPrefixTests
          , elementValueElementKeyTests
           ]

s :: String
s = "identifier"

elementKeyConstruction :: Test
elementKeyConstruction =
  TestList
  [ "globalElementKey" ~:

    ([],s) ~=? (globalElementKey s)

  , "elementKeyPrefixFromString" ~:

    [s] ~=? (elementKeyPrefixFromString s)

  , "elementKeyAsPrefix (without prefix)"  ~:

    [s] ~=? (elementKeyAsPrefix $ globalElementKey s)

  , "elementKeyAsPrefix (with prefix)"  ~:

    ["pfx","leaf"] ~=? (elementKeyAsPrefix $ elementKey ["pfx"] "leaf")
  ]


elementKeyRenderingAndParsing :: Test
elementKeyRenderingAndParsing =
  identityTests
  (elementKeyParse . elementKeyRender)
  "elementKeyRenderingAndParsing"
  [ ("global"            ,globalElementKey "leaf")
  , ("single prefix"     ,elementKey ["prefix"]      "leaf")
  , ("multiple prefixes" ,elementKey ["a","b","cde"] "leaf")
  ]


-------------------------------------------------------------------------------
-- Checks that decoding is the inverse of encoding.
--
-- (We do not need to test the other direction, since
-- we assume that we always start with the "internal representation" -
-- i.e., the un-encoded version.)
-------------------------------------------------------------------------------
elementValueElementKeyPrefixTests :: Test
elementValueElementKeyPrefixTests =
  identityTests
  (elementValueDecodeElementKeyPrefix . elementValueEncodeElementKeyPrefix)
  "elementValueElementKeyPrefix"
  TD.elementKeyPrefixes


-------------------------------------------------------------------------------
-- Checks that decoding is the inverse of encoding.
--
-- (We do not need to test the other direction, since
-- we assume that we always start with the "internal representation" -
-- i.e., the un-encoded version.)
-------------------------------------------------------------------------------
elementValueElementKeyTests :: Test
elementValueElementKeyTests =
  identityTests
  (elementValueDecodeElementKey . elementValueEncodeElementKey)
  "elementValueElementKey"
  [ ("global"        ,globalElementKey "x")
  , ("single prefix" ,elementKey ["prefix"]      "leaf")
  , ("multi"         ,elementKey ["a","b","cde"] "leaf")
  ]

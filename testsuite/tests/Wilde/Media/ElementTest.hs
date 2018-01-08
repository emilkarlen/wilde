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

-- | Tests of the module 'UserInteractionMedia'.
module Wilde.Media.ElementTest
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import TestResources.TestUtils
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

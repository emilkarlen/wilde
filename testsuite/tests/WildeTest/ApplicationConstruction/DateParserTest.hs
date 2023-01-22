module WildeTest.ApplicationConstruction.DateParserTest
       (
         theTest,
       )
       where

import Data.Time.Calendar
import Data.Time.Format

import System.Locale hiding (TimeLocale, defaultTimeLocale)

import qualified Wilde.ApplicationConstruction.UserInteraction.Input.DateParser

import Test.HUnit

-- | Locale to use for all tests.
timeLocale :: TimeLocale
timeLocale = defaultTimeLocale

-- | All tests in this module.
theTest =
  TestList
  [ testNormalFormatting
  , testConstantDayOfMonth
  , testRelativeDayOfMonth
  , testRelativeMonth
  , testRelativeYear
  , testAbsluteAndRelative0
  , testSpaces
  ]

testNormalFormatting :: Test
testNormalFormatting = TestLabel "normal formatting" $ TestList
                       [
                         testStdParse (2012,11,19),
                         testStdParse (1972,4,29)
                       ]

testConstantDayOfMonth :: Test
testConstantDayOfMonth = TestLabel "a constant day-of-month" $ TestList
                         [
                           tG "1"    (2012,11,1),
                           tG "0001" (2012,11,1),
                           tG "10"   (2012,11,10),
                           tG "40"   (2012,11,30)
                         ]

testRelativeDayOfMonth :: Test
testRelativeDayOfMonth = TestLabel "a relative day-of-month" $ TestList
                        [
                          tG "0"    (2012,11,19),
                          tG "000"  (2012,11,19),
                          tG "+0"   (2012,11,19),
                          tG "+000" (2012,11,19),
                          tG "+10"  (2012,11,29),
                          tG "-10"  (2012,11,9)
                        ]

testAbsluteAndRelative0 :: Test
testAbsluteAndRelative0 = TestLabel "some absolute and relative 0" $ TestList
                        [
                          tG "0-0-0"     (2012,11,19),
                          tG "0-10-3"    (2012,10,3),
                          tG "0-0-3"     (2012,11,3),
                          tG "2011-10-0" (2011,10,19),
                          tG "2011-0-0"  (2011,11,19),
                          tG "0-10-0"    (2012,10,19),
                          tG "2011-0-3"  (2011,11,3)
                        ]

testRelativeMonth :: Test
testRelativeMonth =
  TestLabel "relative month" $ TestList
  [ tG "0-0"    (2012,11,19)
  , tG "0-1"    (2012,11,1)
  , tG "+1-2"   (2012,12,21)
  , tG "-1-2"   (2012,10,17)
  ]

testRelativeYear :: Test
testRelativeYear =
  TestLabel "relative year" $ TestList
  [ tG "0-0-0"    (2012,11,19)
  , tG "+2-0-1"   (2014,11,20)
  , tG "-2-0-1"   (2010,11,18)
  ]

testSpaces :: Test
testSpaces = TestLabel "space" $ TestList
             [
               tG " 5   "          (2012,11,5),
               tG " 2011 - 5 -0  " (2011,5,19)
             ]

testStdParse :: (Integer,Int,Int) -> Test
testStdParse gregorianDate@(y,m,d) = tG s gregorianDate
  where
    s = formatTime timeLocale Wilde.ApplicationConstruction.UserInteraction.Input.DateParser.format $ fromGregorian y m d

tG :: String            -- ^ String to parse
   -> (Integer,Int,Int) -- ^ Expected result
   -> Test
tG s g = TestCase $ aG s g

aG :: String -> (Integer,Int,Int) -> Assertion
aG s g = assertEqual s (Right g) (testFunG s)


baseDateG :: (Integer,Int,Int)
baseDateG = (2012,11,19)

baseDate :: Day
baseDate = fromGregorian 2012 11 19

testFun :: String -> Wilde.ApplicationConstruction.UserInteraction.Input.DateParser.ParseResult Day
testFun = Wilde.ApplicationConstruction.UserInteraction.Input.DateParser.parseEmlFormat timeLocale Nothing baseDate

testFunG :: String -> Wilde.ApplicationConstruction.UserInteraction.Input.DateParser.ParseResult (Integer,Int,Int)
testFunG = fmap toGregorian . testFun

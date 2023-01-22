{-# LANGUAGE FlexibleContexts #-}

-- | A function for parsing 'Day's conveniently.
module Wilde.ApplicationConstruction.UserInteraction.Input.DateParser
       (
         ParseResult,
         format,
         parseEmlFormat,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Char

--import System.Locale
import Data.Time.Calendar
import Data.Time.Format

import Text.Parsec


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type ParseResult a = Either String a

-- | The format used by the parser for parsing normally formatted dates.
--
-- This format should be used to generate string representations using
-- 'Date.Time.Format.formatTime' if the string should be parsable
-- by 'parseEmlFormat'.
format :: String
format =  "%F"

parseEmlFormat :: TimeLocale
               -> Maybe String -- ^ A format.  If 'Nothing' than local format ("%F") is used.
               -> Day          -- ^ Base date for eml parsing if parsing according to format fails.
               -> String       -- ^ The string to parse.  Surrounding spaces are allowed.
               -> ParseResult Day
parseEmlFormat timeLocale mbFormatString baseDay s =
  let
    formatString = maybe format id mbFormatString
  in
   maybe
   (emlParse $ filter (not . isSpace) s)
   Right
   (parseTimeM True timeLocale formatString s)
  where
    emlParse :: String -- ^ input without any whitespace
             -> ParseResult Day
    emlParse ('+':diffString) = diff (+) diffString
    emlParse ('-':diffString) = diff (-) diffString
    emlParse s                = do dateComponents <- parseDateParts s
                                   let (by,bm,bd) = toGregorianInteger baseDay
                                   let (y,m,d)    = fill (by,bm) dateComponents
                                   let (y',m',d') = (absolute y by,
                                                     absolute m bm,
                                                     absolute d bd)
                                   return $ fromGregorianInteger (y',m',d')

    -- Replaces the \"relative\" value 0 with it's absolute counterpart.
    absolute :: Integer -> Integer -> Integer
    absolute 0 n = n
    absolute x _ = x

    diff :: (Integer -> Integer -> Integer) -> String -> ParseResult Day
    diff oper diffString =
      do
        dateComponents <- parseDateParts diffString
        let (dy,dm,dd) = fill (0,0) dateComponents
        let (by,bm,bd) = toGregorianInteger baseDay
        return $ fromGregorianInteger (by `oper` dy,
                                       bm `oper` dm,
                                       bd `oper` dd)


toGregorianInteger :: Day -> (Integer,Integer,Integer)
toGregorianInteger day =
  let
    (y,m,d) = toGregorian day
  in
   (y,fromIntegral m,fromIntegral d)

fromGregorianInteger :: (Integer,Integer,Integer) -> Day
fromGregorianInteger (y,m,d) = fromGregorian y (fromIntegral m) (fromIntegral d)

fill :: (Integer,Integer)
     -> [Integer] -- list to fill. length is 1 .. 3.
     -> (Integer,Integer,Integer)
fill (y,m) [d]     = (y,m,d)
fill (y,_) [m,d]   = (y,m,d)
fill (_,_) [y,m,d] = (y,m,d)
fill _     l       = error $ "invalid length of list: " ++ show l

-- | Gives a list of length 1 .. 3
parseDateParts :: String
               -> ParseResult [Integer]
parseDateParts s = either (Left . show) Right $ parse datePartsParser theSourceName s
  where
    theSourceName = "" -- Name of source file, only used for error messages.

-- | Parses the parts of a date: a list of length 1 .. 3.
--
-- The stream must contain only the date parts, no white space anywhere.
datePartsParser :: Stream s m Char => ParsecT s u m [Integer]
datePartsParser =
  do
    x <- number4
    xs <- maxCount 2 followingPart
    eof
    return $ x : xs

number4 :: Stream s m Char => ParsecT s u m Integer
number4 =
  do
    digits <- maxCount1 3 digitAsInt
    return $ fromIntegral $ positionalNumber 10 digits

followingPart :: Stream s m Char => ParsecT s u m Integer
followingPart =
  do
    char '-'
    number4

maxCount :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
maxCount n p =
  if n <= 0
     then return []
  else
    do
      mbX  <- optionMaybe p
      case mbX of
        Nothing -> return []
        Just x  -> do
          xs <- maxCount (n-1) p
          return $ x : xs

maxCount1 :: Stream s m t
          => Int -- ^ The max number of optional elements (the first is non-optional)
          -> ParsecT s u m a
          -> ParsecT s u m [a]
maxCount1 n p =
  do
    x <- p
    xs <- maxCount n p
    return $ x :xs

digitAsInt :: Stream s m Char => ParsecT s u m Int
digitAsInt = fmap digitToInt digit

positionalNumber :: Int   -- ^ base
                 -> [Int] -- ^ Value of digit 0 .. (base - 1)
                 -> Int
positionalNumber base = foldl (\soFar d -> base * soFar + d) 0

module Wilde.ApplicationConstruction.UserInteraction.Input.SyntaxCheck where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Char as Char
import Data.List as L
import qualified Wilde.Utils.Monad as MU


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------

      
checkString :: [String -> Maybe String] -> String -> Bool
checkString checks s = maybe False null (checks MU.=<<< s)

checkDate :: String -> Bool
checkDate = checkString datePartChecks

type PrefixCharPredicateAndPrfixCheck = (Char -> Bool, String -> Bool)

isYear, isMonth, isDay, isDateSeparator :: PrefixCharPredicateAndPrfixCheck

isYear = (Char.isDigit, lengthEquals 4)
isMonth = (Char.isDigit, lengthEquals 2)
isDay = isMonth
isDateSeparator = ((=='-'), lengthEquals 1)

lengthEquals :: Int -> String -> Bool
lengthEquals n s = L.length s == n

datePartChecks :: [String -> Maybe String]
datePartChecks = map checkPrefix [isYear, isDateSeparator, isMonth,isDateSeparator, isDay]

checkPrefix :: PrefixCharPredicateAndPrfixCheck -> String -> Maybe String
checkPrefix (isValidChar, isValidPrefix) s =
    if isValidPrefix prefix
    then Just rest
    else Nothing
  where
    (prefix, rest) = L.span isValidChar s

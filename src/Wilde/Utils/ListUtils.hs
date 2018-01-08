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

-- | Utilities related to lists.
module Wilde.Utils.ListUtils
       (
         firstNonNull,
         
         stringSep,
         
         splitAs,
         splitAs2,
         splitOn,
         splitOnBy,
         -- * Lists of strings
         renderSingleCharSepaList,
         parseSingleCharSepaList,

         -- ** Comma separated lists
         renderCommaList,
         parseCommaList,
         
         -- * Mics
         
         orderAccordingToList,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.List


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Concatenates a list of strings and puts a given separator between each element.
-------------------------------------------------------------------------------
stringSep :: String -> [String] -> String
stringSep sep = concat . intersperse sep

-------------------------------------------------------------------------------
-- | Returns the first non-null (non-empty) list in the given list of lists.
-- If there is non, then the fallback is returned.    
-------------------------------------------------------------------------------
firstNonNull :: [a] -> [[a]] -> [a]
firstNonNull fallback []           = fallback
firstNonNull fallback ([]:ys)      = firstNonNull fallback ys
firstNonNull fallback (l@(x:xs):_) = l
    
-------------------------------------------------------------------------------
-- | Splits the elements of a list, with the length of the first
-- part is the same as that of a given list.
--
-- Gives Nothing if the list to split contains too few elements - i.e. fewer
-- than the template list.
-------------------------------------------------------------------------------
splitAs :: [a]    -- ^ length of this list is the length of the first part.
           -> [b] -- ^ The list to split.
           -> Maybe ([b],[b])
splitAs template ys = splitAs' template ([],ys)
  where
    splitAs' :: [a] -> ([b],[b]) -> Maybe ([b],[b])
    splitAs' []     (taken,rest) = Just (reverse taken,rest)
    splitAs' (x:xs) (_,[])       = Nothing
    splitAs' (x:xs) (taken,y:ys) = splitAs' xs (y:taken,ys)

-------------------------------------------------------------------------------
-- | Splits a list in parts that have the same length as that of the
-- sub lists of a given list.
--
-- Gives 'Nothing if the list to split contains too few elements.
--
-- The result of a successful split ('Just'), is a pair
-- * list of values from the list to split, with the same structure as the
--   first argument.
-- * the values at the end of the list to split, that was not
--   consumed.
--
-- If we have
--
-- > splitAs2 ass bs => Just (bss,unconsumed)
--
-- then the following holds
--
-- > length ass == length bss
--
-- > bs == concat bss ++ unconsumed
--
-- > length (ass !! i) == length (bss !! i)
--
-------------------------------------------------------------------------------
splitAs2 :: [[a]]    -- ^ Determines the lengths of each part.
           -> [b] -- ^ The list to split.
           -> Maybe ([[b]],[b])
splitAs2 template ys = splitAs2' template ([],ys)
  where
    splitAs2' :: [[a]] -> ([[b]],[b]) -> Maybe ([[b]],[b])
    splitAs2' []     (taken,rest) = Just (reverse taken,rest)
    splitAs2' (x:xs) (taken,rest) = do (pfx,sfx) <- splitAs x rest
                                       splitAs2' xs (pfx:taken,sfx)

-------------------------------------------------------------------------------
-- | Special case of 'splitOnBy'.
-------------------------------------------------------------------------------
splitOn :: Eq a
        => a -- ^ The border element.
        -> [a]
        -> [[a]]
splitOn e = splitOnBy (==e)

-------------------------------------------------------------------------------
-- | Splits a list into the sequences that are surrounded by \"border\"
-- elements.
-------------------------------------------------------------------------------
splitOnBy :: (a -> Bool) -- ^ True iff the element is a \"border\" element.
          -> [a] 
          -> [[a]]

splitOnBy isBorder xs = case break isBorder xs of
  ([],[])     -> []
  (part,[])   -> [part]
  (part,x:xs) -> case splitOnBy isBorder xs of
    []  -> [part,[]]
    xss -> part : xss


-------------------------------------------------------------------------------
-- - comma separated lists -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Renders a list of Strings as a single string, just by interspersing commas.
-- 
-- Empty element are ignored (the main reason for this is that
-- the current implementation of 'parseCommaList' is unable to
-- reproduce such elements correctly.)
--
-- Each element must NOT contain a comma (',').
-------------------------------------------------------------------------------
renderCommaList :: [String] -> String
renderCommaList = renderSingleCharSepaList ','

parseCommaList :: String -> [String]
parseCommaList = parseSingleCharSepaList ','

-------------------------------------------------------------------------------
-- | Renders a list of strings, as separated by a given character.
--
-- Empty element are ignored (the main reason for this is that
-- the current implementation of 'parseCommaList' is unable to
-- reproduce such elements correctly.)
--
-- For this function to be invertible by parsing, none of the
-- elements may contain the separation character!
-------------------------------------------------------------------------------
renderSingleCharSepaList :: Char -> [String] -> String
renderSingleCharSepaList sepaChar = concat . 
                                    intersperse [sepaChar] . 
                                    filter (not . null)

-------------------------------------------------------------------------------
-- | Parses a list of strings, as rendered by 'renderSingleCharSepaList'.
--
-- NOTE: A list is only invertible by parsing if none of it's
-- values contain the sepa character!
-------------------------------------------------------------------------------
parseSingleCharSepaList :: Char -> String -> [String]
parseSingleCharSepaList sepaChar = splitOn sepaChar 

-------------------------------------------------------------------------------
-- | Uses a list (\"specification\") to order the elements of another
-- list (\"unordered\")
--
-- Function are supplied that extracts a key element from
-- each element.  The key type is common to \"specification\" and
-- \"unordered\".
--
-- The result is the concatenation of: replacing each element in
-- \"specification\" with (filtering \"unordered\" with matching elements)
-- (matching <=> same key).
--
-- Result is (pseudo):
-- @
--  concat $ map (\key -> filter (with_key key) (unordered)) specification
-- @
--
-- Complexity: O(n2)
-------------------------------------------------------------------------------
orderAccordingToList :: Eq key
                     => (a -> key)
                     -> (b -> key)
                     -> [a]
                     -> [b]
                     -> [b]
orderAccordingToList getSpecKey getUnordKey specification unordered =
  concat $ map filterUnorderedElementsThatMatchesSpecification specification
  where
    filterUnorderedElementsThatMatchesSpecification spec =
      filter (\unord -> getSpecKey spec == getUnordKey unord) unordered

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

-- | Misc utilities.
--
-- (May decompose this module when/if a pattern emerges.)
module Wilde.Utils.Utils 
       (
         -- * Table layout
         
         table,
         fill,
         
         -- * Other
         
         eitherMap,
         readCompletelyAndUnambigously,
         Mismatch(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.List


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Records a mismatch of two values of the same type.
data Mismatch a = Mismatch
                  {
                    actual   :: a,
                    expected :: a
                  }
                  deriving Show

instance Functor Mismatch where
  fmap f (Mismatch a e) = Mismatch (f a) (f e)

eitherMap :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
eitherMap fl fr (Left a)  = Left (fl a)
eitherMap fl fr (Right b) = Right (fr b)

-------------------------------------------------------------------------------
-- | Reads a value from a string.
-- The string must be consumed completely by the reading,
-- and there must be only one parse of the string.
-------------------------------------------------------------------------------
readCompletelyAndUnambigously :: Read a => String -> Maybe a
readCompletelyAndUnambigously s = case readsPrec 0 s of
  [(v,"")] -> Just v
  _        -> Nothing


-------------------------------------------------------------------------------
-- - layout -
-------------------------------------------------------------------------------


columnLengths :: [[[a]]] -> [Int]
columnLengths rows = columnLengths' rows []
  where
    columnLengths' :: [[[a]]] -> [Int] -> [Int]
    columnLengths' []         lengths = lengths
    columnLengths' (row:rows) lengths = columnLengths' rows (updateLengths row lengths)
  
    updateLengths :: [[a]] -> [Int] -> [Int]
    updateLengths [] lengths        = lengths
    updateLengths (col:cols) []     = length col         : updateLengths cols []
    updateLengths (col:cols) (l:ls) = max (length col) l : updateLengths cols ls

table :: a -> [a] -> [[[a]]] -> [[a]]
table _ _ [] = []
table fillValue columnSeparator rows =
  case columnLengths rows of
    []      -> []
    lengths -> map (layoutRow fillValue columnSeparator lengths) rows
      
      where
        layoutRow :: a -> [a] -> [Int] -> [[a]] -> [a]
        layoutRow fill colSepa lengths row = intercalate colSepa $ map (layoutCell fill) $ zip lengths row
        
        layoutCell :: a -> (Int,[a]) -> [a]
        layoutCell fill (cellWidth,cell) = cell ++ replicate (cellWidth - length cell) fill
  
-- | Fills a list to a given size.
--
-- If the list is longer than the size, the result is the original list.
fill :: a -> Int -> [a] -> [a]
fill fillVal len l =
  let
    currLen = length l
  in
   if currLen >= len
   then l
   else l ++ replicate (len - currLen) fillVal

-- | A type for a list that is garrantied to be non-empty.
--
-- Import qualified, as many names clash with those for normal lists.
module Wilde.Utils.NonEmptyList
       (
         List(..),
         cons,
         singleton,
         toList,
         mk,
         fromList,
         applyOnNonEmpty,
         head,
         tail,
         mapM,
         (++),
         concat,
         zip,
         foldl1,
         foldr,
         length,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Prelude as P


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


newtype List a = List (a,[a])
                 deriving (P.Eq,P.Ord,P.Show,P.Read)

length :: List a -> P.Int
length (List (_,xs)) = 1 P.+ P.length xs

instance P.Functor List where
  fmap f (List (x,xs)) = List (f x,P.map f xs)

mapM :: P.Monad m
     => (a -> m b)
     -> List a
     -> m (List b)
mapM f (List (x,xs)) =
  do
    b  <- f x
    bs <- P.mapM f xs
    P.pure P.$ mk b bs

cons :: a -> List a -> List a
cons x (List (y,ys)) = List (x,y:ys)

-- | Never fails.
head :: List a -> a
head (List (x,_)) = x

tail :: List a -> [a]
tail (List (_,xs)) = xs

-- TODO: cons :: a -> List a -> List a

-- | Constructs an list with a single element.
singleton :: a -> List a
singleton x = List (x,[])

(++) :: List a -> [a] -> List a
(++) (List (x,xs)) xs' = List (x,xs P.++ xs')

concat :: List (List a) -> List a
concat (List (List (x,xs),ls)) = List (x,concat' xs ls)
  where
    concat' :: [a] -> [List a] -> [a]
    concat' (x:xs) xss               = x : concat' xs xss
    concat' [] ((List (x,xs)) : xss) = x : concat' xs xss
    concat' [] []                    = []

zip :: List a -> List b -> List (a,b)
zip (List (aH,aT)) (List (bH,bT)) = List ((aH,bH),P.zip aT bT)

applyOnNonEmpty :: ([a] -> b) -> (List a) -> b
applyOnNonEmpty f = f P.. toList

foldl1 :: (a -> a -> a) -> (List a) -> a
foldl1 f = applyOnNonEmpty (P.foldl1 f)

foldr :: (a -> b -> b) -> b -> List a -> b
foldr f x = P.foldr f x P.. toList

toList :: List a -> [a]
toList (List (x,xs)) = x : xs

mk :: a -> [a] -> List a
mk head tail = List (head, tail)

fromList :: [a] -> P.Maybe (List a)
fromList [] = P.Nothing
fromList (x:xs) = P.Just P.$ mk x xs


l = [1,2,3]
mbNe = fromList l
mbNe2 = fromList [1,2,3]

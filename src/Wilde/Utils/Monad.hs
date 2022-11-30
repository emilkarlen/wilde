module Wilde.Utils.Monad where

(=<<<) :: (Foldable t, Monad m) => t (a -> m a) -> a -> m a
(=<<<) ms x = foldl (>>=) (pure x) ms

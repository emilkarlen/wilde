-- | The EMPTY class.
module Wilde.Utils.Empty where

-- | Types with an empty value.
-- (perhaps there is a predefined class with similair functionallity).
class EMPTY a where
    empty :: a

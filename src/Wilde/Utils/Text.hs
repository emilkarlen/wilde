module Wilde.Utils.Text where

import Data.Text
import Data.String

showText :: Show a => a -> Text
showText = fromString . show

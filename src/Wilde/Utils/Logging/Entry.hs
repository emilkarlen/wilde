module Wilde.Utils.Logging.Entry where

import Data.Text

data Level = LIBRARY | DEBUG | INFO | WARNING | ERROR
  deriving (Show, Eq, Enum, Ord)

type Entry = (Level, Text, Maybe Text)

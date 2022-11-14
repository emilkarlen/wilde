module Wilde.Utils.Logging.Entry where

data Level = LIBRARY | DEBUG | INFO | WARNING | ERROR
  deriving (Show, Eq, Enum, Ord)

type Entry = (Level, String, Maybe String)

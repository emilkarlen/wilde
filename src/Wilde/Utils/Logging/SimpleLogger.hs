{-# LANGUAGE OverloadedStrings #-}
module Wilde.Utils.Logging.SimpleLogger
(
  newLogger
)
where

import Control.Monad
import Data.Text
import Wilde.Utils.Logging.Class

newLogger :: (Text -> IO ()) -> Level -> Text -> AnyLogger
newLogger writeLn level prefix =
  AnyLogger $ Setup (writeLn, level, prefix)

newtype Setup = Setup (Text -> IO (), Level, Text)

instance Logger Setup where
    register (Setup (writeLn, level0, prefix)) (level, header, mbBody) = do
        when (level >= level0) $ do
          writeLn $ levelStr level <> prefix <> header
          maybe (pure ()) writeBody mbBody

      where
        writeBody :: Text -> IO ()
        writeBody s = do
            writeLn "--------------------------------------"
            writeLn s
            writeLn "======================================"

    subLogger (Setup (write, level, prefix)) = Setup (write, level, prefix <> "  ")

levelStr :: Level -> Text
levelStr LIBRARY = "LIBRARY "
levelStr DEBUG   = "DEBUG   "
levelStr INFO    = "INFO    "
levelStr WARNING = "WARNING "
levelStr ERROR   = "ERROR   "

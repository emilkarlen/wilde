module Wilde.Utils.Logging.SimpleLogger
(
  newLogger
)
where

import Control.Monad
import Wilde.Utils.Logging.Class

newLogger :: (String -> IO ()) -> Level -> String -> AnyLogger
newLogger writeLn level prefix =
  AnyLogger $ Setup (writeLn, level, prefix)

newtype Setup = Setup (String -> IO (), Level, String)

instance Logger Setup where
    register (Setup (writeLn, level0, prefix)) (level, header, mbBody) = do
        when (level >= level0) $ do
          writeLn $ levelStr level ++ prefix ++ header
          maybe (pure ()) writeBody mbBody

      where
        writeBody :: String -> IO ()
        writeBody s = do
            writeLn "--------------------------------------"
            writeLn s
            writeLn "======================================"

    subLogger (Setup (write, level, prefix)) = Setup (write, level, prefix ++ "  ")

levelStr :: Level -> String
levelStr LIBRARY = "LIBRARY "
levelStr DEBUG   = "DEBUG   "
levelStr INFO    = "INFO    "
levelStr WARNING = "WARNING "
levelStr ERROR   = "ERROR   "

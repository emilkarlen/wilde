{-# LANGUAGE ExistentialQuantification #-}
module Wilde.Utils.Logging.Class
(
    Logger(..),
    AnyLogger(..),
    module Wilde.Utils.Logging.Entry,
)
where

import Wilde.Utils.Logging.Entry


class Logger a where
   register  :: a -> Entry -> IO ()
   subLogger :: a -> a

data AnyLogger = forall l. Logger l => AnyLogger l

instance Logger AnyLogger where
    register (AnyLogger logger) = register logger
    subLogger (AnyLogger logger) = AnyLogger $ subLogger logger

module Wilde.Utils.Logging where

class Monad m => MonadWithLogging m where
    logg :: String -> m ()
    loggBeginEnd :: String -> m a -> m a
    loggBeginEnd entity action = do
        logg (entity ++ " BEGIN")
        result <- action
        logg (entity ++ " END")
        pure result



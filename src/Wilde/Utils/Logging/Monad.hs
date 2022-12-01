{-# LANGUAGE OverloadedStrings #-}
module Wilde.Utils.Logging.Monad
(
    module Wilde.Utils.Logging.Entry,
    MonadWithLogging(..),
)
where

import Control.Monad.IO.Class
import Data.Text

import Wilde.Utils.Logging.Entry
import Wilde.Utils.Logging.Class

class MonadIO m => MonadWithLogging m where
    getLogger :: m AnyLogger

    withLogger :: AnyLogger -> m a -> m a

    withSubLogger :: m a -> m a
    withSubLogger action =
        do
            AnyLogger logger <- getLogger
            let theSubLogger = AnyLogger $ subLogger logger
            withLogger theSubLogger action

    logg_ :: Level -> Text -> Maybe Text -> m ()
    logg_ level header mbBody =
        do
            AnyLogger logger <- getLogger
            liftIO $ register logger (level, header, mbBody)

    logg :: Level -> Text -> m ()
    logg level header = logg_ level header Nothing

    loggBeginEnd :: Level -> Text -> m a -> m a
    loggBeginEnd level entity action =
        do
            AnyLogger logger <- getLogger
            liftIO $ register logger (level, entity <> " BEGIN", Nothing)
            result <- action
            liftIO $ register logger (level, entity <> " END", Nothing)
            pure result

    loggBeginEnd_sub :: Level -> Text -> m a -> m a
    loggBeginEnd_sub level entity action = loggBeginEnd level entity (withSubLogger action)

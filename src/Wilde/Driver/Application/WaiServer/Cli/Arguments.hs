{-# LANGUAGE StrictData #-}

module Wilde.Driver.Application.WaiServer.Cli.Arguments where

import qualified Wilde.Utils.Logging.Entry as Logging

data Arguments = Arguments
    {
        port           :: Maybe Int
    ,   loggingEnabled :: Maybe Bool
    ,   loggingLevel   :: Maybe Logging.Level
    ,   confFile       :: Maybe FilePath
    ,   printDefaults :: Bool
    ,   printConfHelp :: Bool
    }
    deriving (Eq, Show)

module TestResources.Environment where

import qualified Wilde.Media.Database.Configuration as DbConf
import qualified Wilde.Utils.Logging.Class as Logger

emptyDbConfig :: DbConf.Configuration
emptyDbConfig = error "This env (emptyEnv) does not have access to database connections"

emptyLogging :: Logger.AnyLogger
emptyLogging = error "This env (emptyEnv) does not have access to logging"


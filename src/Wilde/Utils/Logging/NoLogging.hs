module Wilde.Utils.Logging.NoLogging
(
  theLogger
)
where

import Wilde.Utils.Logging.Class

theLogger :: AnyLogger
theLogger = AnyLogger $ Setup ()

newtype Setup = Setup ()

instance Logger Setup where
    register setup entry = pure ()

    subLogger setup = setup

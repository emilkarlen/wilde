-- | Configuration read via CLI arguments
-- (including from config files referenced via CLI arguments).

{-# LANGUAGE StrictData #-}

module Wilde.Driver.Application.WaiServer.Cli.Configuration
(
    CliFileHandlingConfiguration(..),
    CliConfiguration(..),
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Driver.Application.WaiServer.RequestHandling.Main.RequestTypeResolving as RTR
import qualified Wilde.Driver.Application.WaiServer.RequestHandling.File.Types as FileTypes


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Configuration of serving of files.
data CliFileHandlingConfiguration =
  CliFileHandlingConfiguration
  {
    paths             :: Maybe RTR.PathPrefixesSetup
  , handledMimeTypes  :: Maybe FileTypes.MimeTypeMapping
  }

-- | All configuration read via CLI.
data CliConfiguration = CliConfiguration
    {
        port         :: Maybe Int
    ,   fileHandling :: CliFileHandlingConfiguration
    }

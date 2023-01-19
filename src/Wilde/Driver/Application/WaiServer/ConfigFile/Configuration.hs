{-# LANGUAGE StrictData #-}

module Wilde.Driver.Application.WaiServer.ConfigFile.Configuration
(
    Configuration(..),
    Files(..),
    Logging(..),

    FilePaths(..),
    MimeTypeMapping(..),

    empty,
    empty_files,
    empty_logging,
)
 where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Driver.Application.WaiServer.RequestHandling.Types (FilePaths)
import Wilde.Driver.Application.WaiServer.RequestHandling.File.Types (MimeTypeMapping)
import Wilde.Utils.Logging.Entry (Level)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Configuration that can be read from a config file.
data Configuration =
    Configuration
    {
        port          :: Maybe Int
    ,   files         :: Maybe Files
    ,   logging       :: Maybe Logging
    }
    deriving (Eq, Show)

data Files =
    Files
    {
        file_paths    :: Maybe FilePaths
    ,   mime_types    :: Maybe MimeTypeMapping
    }
    deriving (Eq, Show)

data Logging =
    Logging
    {
        enabled    :: Maybe Bool
    ,   level      :: Maybe Level
    }
    deriving (Eq, Show)

-- | A `Configuration` with no values set
empty :: Configuration
empty = Configuration Nothing Nothing Nothing

empty_files :: Files
empty_files = Files Nothing Nothing

empty_logging :: Logging
empty_logging = Logging Nothing Nothing

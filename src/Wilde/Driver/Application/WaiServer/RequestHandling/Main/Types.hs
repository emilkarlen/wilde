{-# LANGUAGE StrictData #-}

module Wilde.Driver.Application.WaiServer.RequestHandling.Main.Types
(
    FileHandlingConfiguration(..),
    RequestPathsConfiguration(..),
    MainConfiguration(..),
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Driver.Application.Web.Types (CodingConfiguration, RequestPath)
import           Wilde.Driver.Application.WaiServer.RequestHandling.Types (RequestHandlerResolver, FilePaths)

import qualified Wilde.Driver.Application.WaiServer.RequestHandling.File.Types as FileTypes


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Configuration of serving of files.
data FileHandlingConfiguration =
  FileHandlingConfiguration
  {
    filePaths         :: FilePaths
    -- ^ The request paths where files are served
  , handledMimeTypes  :: FileTypes.MimeTypeMapping
    -- ^ File extensions, and corresponding mime type,
    -- that are served. Files with other extensions are
    -- not served.
    -- The file extension is the part of the file name
    -- that follows the last ".".
  }
  deriving (Eq, Show)

-- | Configuration of the request paths
-- that an application should handle.
data RequestPathsConfiguration =
  RequestPathsConfiguration
  {
    services :: RequestPath
    -- ^ The path that serves Wilde services.
  , files    :: FileHandlingConfiguration
    -- ^ Serving of files.
  , fallback :: Maybe RequestHandlerResolver
    -- ^ Tells what to do with request paths that matches
    -- neither services nor files.
  }

-- | Configuration of the web server part of an application.
data MainConfiguration =
  MainConfiguration
  {
    coding       :: CodingConfiguration
  , requestPaths :: RequestPathsConfiguration
  }

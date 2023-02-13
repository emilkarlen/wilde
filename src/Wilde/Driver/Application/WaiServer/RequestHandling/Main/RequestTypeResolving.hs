{-# LANGUAGE StrictData #-}

module Wilde.Driver.Application.WaiServer.RequestHandling.Main.RequestTypeResolving
(
    module Wilde.Driver.Application.Web.Types,

    RequestType(..),
    PathPrefixesSetup(..),
    Result,
    resolve,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Map as M
import Data.List as L

import Wilde.Driver.Application.Web.Types
import Wilde.Driver.Application.WaiServer.RequestHandling.Types (FilePaths)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data RequestType
    = Service
    | File FilePath -- ^ File request with file system path prefix
    deriving (Show, Eq)

data PathPrefixesSetup =
    PathPrefixesSetup
    {
        services :: RequestPath
         -- ^ The request path prefix that serves Wilde services
    ,   files    :: FilePaths
        -- ^ Mapping of request path prefixes to file system path prefixes
    }
    deriving (Eq, Show)

{- | The request together with the path following
the path prefix of the matched "path setup element".
-}
type Result = Maybe (RequestType, RequestPath)

{- | Resolves the request for an actual path

Gives the Request type together with the
suffix of the actual path that comes after
the matching path prefix in the setup.

If no path prefix matches than the request type `Custom`
is given together with the actual=original path.
-}
resolve :: PathPrefixesSetup -> RequestPath -> Result
resolve (PathPrefixesSetup services files) path =
    matchPath sortedPathMappings path
    where
        setupAsMap :: M.Map RequestPath RequestType
        setupAsMap = M.insert services Service $ M.map File files

        sortedPathMappings :: [(RequestPath, RequestType)]
        sortedPathMappings  = M.toDescList setupAsMap

matchPath :: [(RequestPath, RequestType)] -> RequestPath -> Result
matchPath [] path = Nothing
matchPath ((pathPrefix, rt):xs) path =
    case L.stripPrefix pathPrefix path of
        Just suffix -> Just (rt, suffix)
        Nothing     -> matchPath xs path

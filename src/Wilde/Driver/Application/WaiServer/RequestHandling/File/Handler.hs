{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Wilde.Driver.Application.WaiServer.RequestHandling.File.Handler
    (
        Configuration(..),
        resolveRequest,
    )
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.List as L
import Control.Monad
import Data.String as S
import Data.Text as T
import Data.ByteString as BS

import qualified Network.HTTP.Types as HttpTypes

import qualified Network.Wai as Wai

import qualified Wilde.Utils.Logging.Class as Logging
import qualified Wilde.Driver.Application.Web.Utils as Utils

import           Wilde.Driver.Application.WaiServer.RequestHandling.File.Types
import qualified Wilde.Driver.Application.WaiServer.RequestHandling.File.PathHandling as PathHandling
import           Wilde.Driver.Application.WaiServer.RequestHandling.Types (RequestHandlerResolver, ValidRequestHandler)
import           Wilde.Driver.Application.WaiServer.RequestHandling.File.PathHandling (filePathSepa)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data Configuration =
    Configuration
    {
        filePathPrefix :: FilePath
        -- ^ File System prefix for the location of handled files.
    ,   mimeTypes      :: MimeTypeMapping
        -- ^ Mime types for all handled file types.
    }

{- | request:
       pathInfo: the suffix of the matched request path prefix.
       Is empty if the prefix matched the whole path.
-}
resolveRequest :: Logging.AnyLogger -> CodingConfiguration -> Configuration -> RequestHandlerResolver
resolveRequest logger sysConf conf request =
    do
        validatePath
        supportedMimeType <- resolveMimeType
        pure $ serveValidPath logger (filePathPrefix conf) supportedMimeType rqPathSuffix
    where
        rqPathSuffix       :: RequestPath
        rqPathSuffix        = Wai.pathInfo request

        fsPathPrefix       :: FilePath
        fsPathPrefix        = filePathPrefix conf

        validatePath :: HandlerResolvingMonad ()
        validatePath =
            unless (PathHandling.pathIsValid rqPathSuffix)
                   (throwBadRequest $
                    "Invalid path: " <> toText (Wai.rawPathInfo request))

        actualMimeType = PathHandling.resolveMimeTypeFromValidPath (mimeTypes conf) (fsPathPrefix, rqPathSuffix)

        resolveMimeType :: HandlerResolvingMonad MimeType
        resolveMimeType =
            case actualMimeType of
                Left errMsg -> throwBadRequest $ "Invalid file type: " <> errMsg
                Right x     -> pure x

        toText :: BS.ByteString -> T.Text
        toText = queryTDecoder sysConf


-- | Serves a path that has been cheked "statically" -
-- it is possible that the file does not exist or is not a regular file.
--
-- This code asserts that the error conditions are handled by the Wai helper.
serveValidPath
    :: Logging.AnyLogger
    -> FilePath    -- ^ the file system path that is the "root" for the served file (relative path)
    -> MimeType    -- ^ mime type of the file
    -> RequestPath -- ^ the file path of the request
                   -- ^ appended to the FS path prefix, if non-empty
    -> ValidRequestHandler
serveValidPath logger filePathPrefix mimeType requestPath errorHandler responder =
    do
        let path =
                if Prelude.null requestPath
                then filePathPrefix
                else Prelude.concat
                    [
                    filePathPrefix,
                    filePathSepa,
                    toRelativeFilePath requestPath
                    ]
        Logging.register logger (Logging.LIBRARY, Utils.logHdr1 <> " handle file : " <> S.fromString path, Nothing)
        let header   = Utils.singleResponseHeaders mimeType
        let response = Wai.responseFile HttpTypes.ok200 header path Nothing
        responder response

toRelativeFilePath :: RequestPath -> FilePath
toRelativeFilePath parts = T.unpack $ T.concat $ L.intersperse filePathSepa parts

{-# LANGUAGE OverloadedStrings #-}
module Wilde.Driver.Application.WaiServer.RequestHandling.File.Handler where


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
import qualified Wilde.Driver.Application.Utils as Utils

import Wilde.Driver.Application.WaiServer.RequestHandling.File.Types
import qualified Wilde.Driver.Application.WaiServer.RequestHandling.File.PathHandling as PathHandling
import Wilde.Driver.Application.WaiServer.RequestHandling.Types (RequestHandlerResolver, ValidRequestHandler)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


filePathSepa :: IsString a => a
filePathSepa = "/"

data Configuration =
    Configuration
    {
      mimeTypes      :: MimeTypeMapping
      -- ^ Mime types for all handled file types.
    , filePathPrefix :: FilePath
      -- ^ File System prefix for the location of handled files.
    }

resolveRequest :: Logging.AnyLogger -> SystemConfiguration -> Configuration -> RequestHandlerResolver
resolveRequest logger sysConf conf request =
    do
        validatePath
        supportedMimeType <- resolveMimeType
        pure $ serveValidPath logger (filePathPrefix conf) supportedMimeType path
    where
        path       :: RequestPath
        path        = Wai.pathInfo request

        validatePath :: HandlerResolvingMonad ()
        validatePath =
            unless (PathHandling.pathIsValid path)
                   (throwBadRequest $
                    "Invalid path: " <> toText (Wai.rawPathInfo request))

        actualMimeType = PathHandling.resolveMimeTypeFromValidPath (mimeTypes conf) path
    
        toText :: BS.ByteString -> T.Text
        toText = queryTDecoder sysConf

        resolveMimeType :: HandlerResolvingMonad MimeType
        resolveMimeType =
            case actualMimeType of
                Left errMsg -> throwBadRequest $ "Invalid file type: " <> errMsg
                Right x     -> pure x


-- | Serves a path that has been cheked "statically" -
-- it is possible that the file does not exist or is not a regular file.
--
-- This code asserts that the error conditions are handled by the Wai helper.
serveValidPath
    :: Logging.AnyLogger
    -> FilePath    -- ^ the file system path that is the "root" for the served file (relative path) 
    -> MimeType    -- ^ mime type of the file
    -> RequestPath -- ^ the file path of the request
    -> ValidRequestHandler
serveValidPath logger filePathPrefix mimeType requestPath errorHandler responder =
    do
        let path = Prelude.concat
                [
                filePathPrefix, 
                filePathSepa,
                toRelativeFilePath requestPath
                ]
        Logging.register logger (Logging.LIBRARY, "handle file : " <> path, Nothing)
        let header   = Utils.singleResponseHeaders mimeType
        let response = Wai.responseFile HttpTypes.ok200 header path Nothing
        responder response

toRelativeFilePath :: RequestPath -> FilePath
toRelativeFilePath parts = T.unpack $ T.concat $ L.intersperse filePathSepa parts

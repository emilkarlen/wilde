{-# LANGUAGE OverloadedStrings #-}
-- | The main handler of Wai apps run as a server.
module Wilde.Driver.Application.WaiServer.RequestHandling.Main.Handler
       (
         Configuration(..),

         newApplication,
         )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.Text as T
import qualified Wilde.Utils.Text as TU

import qualified Control.Exception as Exception

import qualified Control.Monad.IO.Class as MonadIO

import           Control.Monad.Trans.Except as ExceptT

import           Data.Text
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HttpTypes

import qualified Wilde.Driver.Application.Utils as Utils

import qualified Wilde.Application.ApplicationConfiguration as AppConf

import qualified Wilde.Utils.Logging.Class as Logging

import           Wilde.Driver.Application.Types
import qualified Wilde.Driver.Application.WaiServer.RequestHandling.Main.RequestTypeResolving as RTR
import           Wilde.Driver.Application.WaiServer.RequestHandling.Types (ValidRequestHandler, RequestHandlerResolver, ErrorHandler, WaiResponder)

import qualified Wilde.Driver.Application.WaiServer.RequestHandling.Service.Handler as ServiceHandling
import qualified Wilde.Driver.Application.WaiServer.RequestHandling.File.Handler as FileHandling
import qualified Wilde.Driver.Application.WaiServer.RequestHandling.File.Types as FileTypes


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Complete configuration for a WAI web server application.
data Configuration =
  Configuration
  {
    coding            :: SystemConfiguration
  , paths             :: RTR.PathPrefixesSetup
  , handledMimeTypes  :: FileTypes.MimeTypeMapping
  , customPathHandler :: Maybe RequestHandlerResolver
  }

-- | Constructs a WAI application acting as a web server.
newApplication :: Configuration
               -> AppConf.ApplicationConfiguration
               -> Wai.Application
newApplication warpConf appConf request respond =
    MonadIO.liftIO $
      Exception.catch
      (withinTopLevelErrorHandling warpConf appConf request respond)
      (internalServerErrorHandler appConf (contentTEncoder $ coding warpConf) request respond)

withinTopLevelErrorHandling
  :: Configuration
  -> AppConf.ApplicationConfiguration
  -> Wai.Application
withinTopLevelErrorHandling warpConf appConf request respond =
  do
    loggRequest appConf request
    handlerResolvingResult <- ExceptT.runExceptT $ resolveHandler warpConf appConf request
    either handleInvalidRequest executeHandler handlerResolvingResult

  where
    executeHandler :: ValidRequestHandler -> IO Wai.ResponseReceived
    executeHandler handler = handler (errorHandler appConf respond contentEncoder) respond

    handleInvalidRequest :: HandlingError -> IO Wai.ResponseReceived
    handleInvalidRequest error = errorHandler appConf respond contentEncoder error

    contentEncoder :: TextEncoder
    contentEncoder = contentTEncoder $ coding warpConf

resolveHandler
  :: Configuration
  -> AppConf.ApplicationConfiguration
  -> Wai.Request
  -> HandlerResolvingMonad ValidRequestHandler
resolveHandler warpConf appConf request =
  do
    let requestType = RTR.resolve (paths warpConf) path
    let sysConf = coding warpConf
    case requestType of
      Just (RTR.Service, path)             -> ServiceHandling.resolveRequest sysConf appConf (request {Wai.pathInfo = path})
      Just (RTR.File filePathPrefix, path) -> FileHandling.resolveRequest logger sysConf
                                              (fileResovingConf filePathPrefix)
                                              (request {Wai.pathInfo = path})
      Nothing ->
        case customPathHandler warpConf of
          Just handlerResolver -> handlerResolver request
          Nothing -> throwBadRequest "Bad request - neither service nor file"

  where
    path   = Wai.pathInfo request
    logger = AppConf.appLogger appConf

    fileResovingConf :: FilePath -> FileHandling.Configuration
    fileResovingConf filePathPrefix =
      FileHandling.Configuration
      {
        FileHandling.mimeTypes      = handledMimeTypes warpConf
      , FileHandling.filePathPrefix = filePathPrefix
      }


-------------------------------------------------------------------------------
-- - error handling and reporting -
-------------------------------------------------------------------------------


errorHandler :: AppConf.ApplicationConfiguration -> WaiResponder -> TextEncoder -> ErrorHandler
errorHandler AppConf.ApplicationConfiguration {AppConf.appLogger = logger} respond encoder (HandlingError msg) = do
  Logging.register logger (Logging.LIBRARY, logStr, Nothing)
  respond $ plainTextResponse encoder status msg
  where
    status = HttpTypes.badRequest400

    logStr :: Text 
    logStr = TU.showText status <> " : " <> TU.showText msg

plainTextResponse :: TextEncoder -> HttpTypes.Status -> T.Text -> Wai.Response
plainTextResponse encode status text =
  Wai.responseBuilder
  status
  (Utils.headers_plainText encode)
  (encode text)

-- TODO [WAI] Translate error to user friendly text.
internalServerErrorHandler
  :: AppConf.ApplicationConfiguration
  -> TextEncoder
  -> Wai.Request
  -> WaiResponder
  -> Exception.SomeException
  -> IO Wai.ResponseReceived
internalServerErrorHandler AppConf.ApplicationConfiguration {AppConf.appLogger = logger}  contentEncoder request responder ex = do
  Logging.register logger (Logging.LIBRARY, TU.showText status, Just $ TU.showText ex)
  responder $
    plainTextResponse
    contentEncoder
    status
    ("Internal Server Error: " <> TU.showText ex)
  where
    status = HttpTypes.internalServerError500

badRequestResponse :: TextEncoder
                   -> T.Text -- ^ error message
                   -> Wai.Response
badRequestResponse contentEncoder = plainTextResponse contentEncoder HttpTypes.badRequest400


-------------------------------------------------------------------------------
-- - logging -
-------------------------------------------------------------------------------


loggRequest :: AppConf.ApplicationConfiguration -> Wai.Request -> IO ()
loggRequest AppConf.ApplicationConfiguration {AppConf.appLogger = logger} request =
    Logging.register logger $ requestLogEntry request

requestLogEntry :: Wai.Request -> Logging.Entry
requestLogEntry request =
  (Logging.LIBRARY, Utils.logHdr0 <> " REQUEST " <> method, Just body)
  where
    method   = TU.showText $ Wai.requestMethod request
    pathInfo = "pathInfo    = " <> TU.showText (Wai.pathInfo request)
    queryStr = "queryString = " <> TU.showText (Wai.queryString request)
    body     = pathInfo <> "\n\n" <> queryStr

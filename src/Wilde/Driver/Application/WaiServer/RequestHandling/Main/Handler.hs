{-# LANGUAGE OverloadedStrings #-}
-- | The main handler of Wai apps run as a server.
module Wilde.Driver.Application.WaiServer.RequestHandling.Main.Handler
       (
          module C,

          newApplication,
         )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.Text as T
import qualified Wilde.Utils.Text as TU

import qualified Control.Exception as Exception

import           Control.Monad.Trans.Except as ExceptT

import           Data.Text
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HttpTypes

import qualified Wilde.Driver.Application.Web.Utils as Utils

import qualified Wilde.Application.ApplicationConfiguration as AppConf

import qualified Wilde.Utils.Logging.Class as Logging

import           Wilde.Driver.Application.Web.Types
import qualified Wilde.Driver.Application.WaiServer.RequestHandling.Main.RequestTypeResolving as RTR
import           Wilde.Driver.Application.WaiServer.RequestHandling.Types (ValidRequestHandler, ErrorHandler, WaiResponder)

import qualified Wilde.Driver.Application.WaiServer.RequestHandling.Service.Handler as ServiceHandling
import qualified Wilde.Driver.Application.WaiServer.RequestHandling.File.Handler as FileHandling

import           Wilde.Driver.Application.WaiServer.RequestHandling.Main.Types as C


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Constructs a WAI application acting as a web server.
newApplication :: MainConfiguration
               -> AppConf.ApplicationConfiguration
               -> Wai.Application
newApplication warpConf appConf request respond =
  Exception.catch
  (withinTopLevelErrorHandling warpConf appConf request respond)
  (internalServerErrorHandler appConf (contentTEncoder $ coding warpConf) request respond)

withinTopLevelErrorHandling
  :: MainConfiguration
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
  :: MainConfiguration
  -> AppConf.ApplicationConfiguration
  -> Wai.Request
  -> HandlerResolvingMonad ValidRequestHandler
resolveHandler warpConf appConf request =
  do
    let requestType = RTR.resolve (getPathPrefixesSetup warpConf) path
    let sysConf = coding warpConf
    case requestType of
      Just (RTR.Service, path)             -> ServiceHandling.resolveRequest sysConf appConf (request {Wai.pathInfo = path})
      Just (RTR.File filePathPrefix, path) -> FileHandling.resolveRequest logger sysConf
                                              (fileResovingConf filePathPrefix)
                                              (request {Wai.pathInfo = path})
      Nothing ->
        case C.fallback $ C.requestPaths warpConf of
          Just handlerResolver -> handlerResolver request
          Nothing -> throwBadRequest "Bad request - neither service nor file"

  where
    path   = Wai.pathInfo request
    logger = AppConf.appLogger appConf

    fileResovingConf :: FilePath -> FileHandling.Configuration
    fileResovingConf filePathPrefix =
      FileHandling.Configuration
      {
        FileHandling.mimeTypes      = handledMimeTypes $ files $ requestPaths warpConf
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

internalServerErrorHandler
  :: AppConf.ApplicationConfiguration
  -> TextEncoder
  -> Wai.Request
  -> WaiResponder
  -> Exception.SomeException
  -> IO Wai.ResponseReceived
internalServerErrorHandler AppConf.ApplicationConfiguration {AppConf.appLogger = logger}  contentEncoder request responder ex = do
  Logging.register logger (Logging.ERROR, TU.showText status, Just msg)
  responder $
    plainTextResponse
    contentEncoder
    status
    msg
  where
    status = HttpTypes.internalServerError500
    msg :: T.Text
    msg = "Internal Server Error: " <> TU.showText ex

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


-------------------------------------------------------------------------------
-- - utils -
-------------------------------------------------------------------------------


getPathPrefixesSetup :: MainConfiguration -> RTR.PathPrefixesSetup
getPathPrefixesSetup mc =
  RTR.PathPrefixesSetup
  {
        RTR.services = C.services $ C.requestPaths mc
    ,   RTR.files    = C.filePaths $ C.files $ C.requestPaths mc
  }

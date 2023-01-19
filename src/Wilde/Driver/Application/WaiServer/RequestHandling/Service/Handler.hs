{-# LANGUAGE OverloadedStrings #-}

module Wilde.Driver.Application.WaiServer.RequestHandling.Service.Handler where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.String as S
import Control.Monad.Trans.Except as ExceptT

import qualified Network.Wai as Wai

import qualified Wilde.Utils.Logging.Class as Logging

import qualified Wilde.Render.Cgi.ElementSetIo as ElementSetIo

import qualified Wilde.Application.ApplicationConfiguration as AppConf
import qualified Wilde.Driver.Application.Utils as Utils

import qualified Wilde.Driver.Application.Cgi.CgiHtml as AppCgiHtml
import Wilde.Driver.Application.Types
import Wilde.Driver.Application.Wai.Utils as WaiUtils
import Wilde.Driver.Application.WaiServer.RequestHandling.Types (RequestHandlerResolver, ValidRequestHandler)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


resolveRequest
  :: CodingConfiguration
  -> AppConf.ApplicationConfiguration
  -> RequestHandlerResolver
resolveRequest sysConf appConf request =
  do
    if null requestPath
      then pure $ serviceRequestHandler sysConf appConf request
      else ExceptT.throwE $ HandlingError "Invalid service path"
  where
    requestPath = Wai.pathInfo request

serviceRequestHandler
    :: CodingConfiguration
    -> AppConf.ApplicationConfiguration
    -> Wai.Request
    -> ValidRequestHandler
serviceRequestHandler
  CodingConfiguration
  {
    contentEncoder = theContentEncoder
  , queryVarDecoder = theQueryVarDecoder
  }
  appConf request errorHandler responder =
  do
    let logger = AppConf.appLogger appConf
    Logging.register logger (Logging.LIBRARY, Utils.logHdr1 <> " handle service", Nothing)
    res <- AppCgiHtml.lookupAndRunService appConf rawRequestInput
    case res of
      Left err         -> errorHandler $ HandlingError $ S.fromString $ show err
      Right htmlString -> responder $ WaiUtils.okHtmlResponse theContentEncoder htmlString
  where
    rawRequestInput :: ElementSetIo.ServerVariables
    rawRequestInput = WaiUtils.buildRawInput theQueryVarDecoder request

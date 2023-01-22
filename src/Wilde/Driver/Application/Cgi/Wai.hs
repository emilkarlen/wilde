-- | An interface to execute a Wilde application
-- as a WAI application.
module Wilde.Driver.Application.Cgi.Wai
       (
         -- * Construction of a WAI Application

         ContentEncoder,
         ContentDecoder,
         CodingConfiguration(..),

         newApplication,

         -- * Re-exported from "Wilde.Application.ApplicationConfiguration"

         AppConf.ApplicationConfiguration(..),
         AppConf.ApplicationServices,
         AppConf.Translations(..),
         )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Control.Exception as Exception

import qualified Control.Monad.IO.Class as MonadIO

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HttpTypes


import qualified Wilde.Render.Cgi.ElementSetIo as ElementSetIo

import qualified Wilde.Application.ApplicationConfiguration as AppConf

import qualified Wilde.Driver.Application.Cgi.CgiHtml as AppCgiHtml


import qualified Data.String as String

import qualified Blaze.ByteString.Builder as B

import Wilde.Driver.Application.Types


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


newApplication :: CodingConfiguration
               -> AppConf.ApplicationConfiguration
               -> Wai.Application
newApplication sysConf appConf request respond =
  do
    response <- MonadIO.liftIO $
                Exception.catch
                (exceptionThrowingWaiApp sysConf appConf request)
                (topLevelErrorHandler (contentEncoder sysConf) appConf)
    respond response

exceptionThrowingWaiApp :: CodingConfiguration
                        -> AppConf.ApplicationConfiguration
                        -> Wai.Request
                        -> IO Wai.Response
exceptionThrowingWaiApp (CodingConfiguration
                         {
                           contentEncoder = theContentEncoder
                         , queryVarDecoder = theQueryVarDecoder
                         }) appConf request =
  do
    res <- AppCgiHtml.lookupAndRunService appConf rawRequestInput
    case res of
      Left err -> serviceLookupError theContentEncoder err
      Right htmlString -> pure $ okHtmlResponse theContentEncoder htmlString
  where
    rawRequestInput :: ElementSetIo.ServerVariables
    rawRequestInput = buildRawInput theQueryVarDecoder request

-- Could not lookup the service to execte.  Result is error page.
-- TODO [WAI] Translate error to user friendly text.
serviceLookupError :: ContentEncoder
                   -> AppCgiHtml.ServiceSpecificationError
                   -> IO Wai.Response
serviceLookupError contentEncoder err = pure $
                                        (errorResponse contentEncoder)
                                        HttpTypes.badRequest400
                                        "text/plain" $
                                        "Service Lookup Error: " ++ (show err)

-- TODO [WAI] Translate error to user friendly text.
topLevelErrorHandler :: ContentEncoder
                     -> AppConf.ApplicationConfiguration
                     -> Exception.SomeException
                     -> IO Wai.Response
topLevelErrorHandler contentEncoder appConf ex =
  pure $
  (errorResponse contentEncoder)
  HttpTypes.internalServerError500
  "text/plain" $
  "Internal Server Error: " ++ (show ex)

-- | Simple helper to construct an error response.
-- This is not production code - improve to display
-- more nice error messages!
errorResponse :: ContentEncoder
              -> HttpTypes.Status
              -> String
              -- ^ mime type
              -> String
              -- ^ Page contents
              -> Wai.Response
errorResponse contentEncoder st mimeType pageContents =
  Wai.responseBuilder st headers pageContentBuilder
  where
    headers :: [HttpTypes.Header]
    headers = [(HttpTypes.hContentType, String.fromString mimeType)]

    pageContentBuilder :: B.Builder
    pageContentBuilder = contentEncoder pageContents

-- | Construction of an OK response whos contents is a html page.
okHtmlResponse :: ContentEncoder
               -> String -- ^ HTML
               -> Wai.Response
okHtmlResponse contentEncoder html =
  Wai.responseBuilder st headers pageContentBuilder
  where
    st :: HttpTypes.Status
    st = HttpTypes.ok200

    headers :: [HttpTypes.Header]
    headers = [(HttpTypes.hContentType, mimeType)
              -- ,(HttpTypes.hContentEncoding,String.fromString "UTF-8")
              ]

    pageContentBuilder :: B.Builder
    pageContentBuilder = contentEncoder html

    --mimeType :: HttpTypes.Ascii
    mimeType = String.fromString "text/html"


-------------------------------------------------------------------------------
-- - buildInput -
-------------------------------------------------------------------------------


buildRawInput :: ContentDecoder -> Wai.Request -> ElementSetIo.ServerVariables
buildRawInput contentDecoder request = queryToServerVariables theQueryString
  -- unsafePerformIO $ do
  --   puts "________________________________________"
  --   pure $ queryToServerVariables theQueryString
  where
    theQueryString = Wai.queryString request

    queryToServerVariables :: HttpTypes.Query -> ElementSetIo.ServerVariables
    queryToServerVariables = map tr

    tr (bs,mbBs) = (contentDecoder bs,fmap contentDecoder mbBs)

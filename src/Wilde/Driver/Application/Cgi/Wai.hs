{-
Copyright 2013 Emil Karlén.

This file is part of Wilde.

Wilde is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Wilde is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Wilde.  If not, see <http://www.gnu.org/licenses/>.
-}

-- | An interface to execute a Wilde application
-- as a WAI application.
module Wilde.Driver.Application.Cgi.Wai
       (
         -- * Construction of a WAI Application

         ContentEncoder,
         ContentDecoder,
         SystemConfiguration(..),
         
         csApplication,

         -- * Re-exported from "Wilde.Application.ApplicationConfiguration"

         AppConf.ApplicationConfiguration(..),
         AppConf.ApplicationServices,
         AppConf.Translations(..),
         )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


-- import System.IO.Unsafe -- DEBUG
-- import System.IO -- DEBUG

import qualified Control.Exception as Exception

import qualified Control.Monad.IO.Class as MonadIO

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HttpTypes


import qualified Wilde.Render.Cgi.ElementSetIo as ElementSetIo

import qualified Wilde.Application.ApplicationConfiguration as AppConf

import qualified Wilde.Driver.Application.Cgi.CgiHtml as AppCgiHtml


import qualified Data.String as String

import qualified Data.ByteString as ByteString

import qualified Blaze.ByteString.Builder as B


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type ContentEncoder = String -> B.Builder

type ContentDecoder = ByteString.ByteString -> String

data SystemConfiguration =
  SystemConfiguration
  {
    contentEncoder  :: ContentEncoder
  , queryVarDecoder :: ContentDecoder
  }

-- fromString :: ContentEncoder
-- fromString = BChar8.fromString

csApplication :: SystemConfiguration
              -> AppConf.ApplicationConfiguration
              -> Wai.Application
csApplication sysConf appConf request respond =
  do
    response <- MonadIO.liftIO $
                Exception.catch
                (exceptionThrowingWaiApp sysConf appConf request)
                (topLevelErrorHandler (contentEncoder sysConf) appConf)
    respond response

exceptionThrowingWaiApp :: SystemConfiguration
                        -> AppConf.ApplicationConfiguration
                        -> Wai.Request
                        -> IO Wai.Response
exceptionThrowingWaiApp (SystemConfiguration
                         {
                           contentEncoder = theContentEncoder
                         , queryVarDecoder = theQueryVarDecoder
                         }) appConf request =
  do
    res <- AppCgiHtml.lookupAndRunService appConf rawRequestInput
    case res of
      Left err -> serviceLookupError theContentEncoder err
      Right htmlString -> return $ okHtmlResponse theContentEncoder htmlString
  where
    rawRequestInput :: ElementSetIo.ServerVariables
    rawRequestInput = buildRawInput theQueryVarDecoder request

-- Could not lookup the service to execte.  Result is error page.
-- TODO [WAI] Translate error to user friendly text.
serviceLookupError :: ContentEncoder
                   -> AppCgiHtml.ServiceSpecificationError
                   -> IO Wai.Response
serviceLookupError contentEncoder err = return $
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
  return $
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
  --   return $ queryToServerVariables theQueryString
  where
    theQueryString = Wai.queryString request

    queryToServerVariables :: HttpTypes.Query -> ElementSetIo.ServerVariables
    queryToServerVariables = map tr

    tr (bs,mbBs) = (contentDecoder bs,fmap contentDecoder mbBs)

    -- tr_debug (bs,mbBs) = unsafePerformIO $ do
    --   pf $ contentDecoder bs
    --   pfi $ show $ fmap ByteString.unpack mbBs
    -- När "knepiga" bokstäver skrivs ut på stderr här nedan så smäller det!!??
    --   pfi $ maybe "Nothing" (\s -> "\"" ++ s ++ "\"") $ fmap contentDecoder mbBs
    --   return (contentDecoder bs,fmap contentDecoder mbBs)

-- puts = hPutStrLn stderr

-- frame :: String -> String
-- frame s = "|" ++ s ++ "|"

-- pf s = puts $ frame s
-- pfi s = puts $ "   " ++ frame s

module Wilde.Driver.Application.Wai.Utils where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------



import qualified Data.String as String
import qualified Blaze.ByteString.Builder as B

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HttpTypes

import qualified Wilde.Render.Cgi.ElementSetIo as ElementSetIo

import Wilde.Driver.Application.Web.Types


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


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
              ,(HttpTypes.hContentEncoding,String.fromString "UTF-8")
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
  where
    theQueryString = Wai.queryString request

    queryToServerVariables :: HttpTypes.Query -> ElementSetIo.ServerVariables
    queryToServerVariables = map tr

    tr (bs,mbBs) = (contentDecoder bs,fmap contentDecoder mbBs)

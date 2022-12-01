{-# LANGUAGE OverloadedStrings #-}
module Wilde.Driver.Application.Utils where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.String as S
import           Data.Text
import qualified Data.ByteString as BS
import qualified Blaze.ByteString.Builder as B

import qualified Network.HTTP.Types as HttpTypes

import           Wilde.Driver.Application.Types


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


singleResponseHeaders :: MimeType
                      -> [HttpTypes.Header]
singleResponseHeaders mimeType = [(HttpTypes.hContentType, S.fromString mimeType)]

headers_plainText :: TextEncoder
                  -> [HttpTypes.Header]
headers_plainText encoder = [(HttpTypes.hContentType, encodedMimeType)]
  where
    encodedMimeType :: BS.ByteString
    encodedMimeType = B.toByteString $ encoder mimeType_textPlain

mimeType_textPlain :: Text
mimeType_textPlain = "text/plain"


-------------------------------------------------------------------------------
-- - logging -
-------------------------------------------------------------------------------


logHdr0, logHdr1 :: Text
logHdr0 = logHdr1 <> logHdr1
logHdr1 = ">>>>>>>>"

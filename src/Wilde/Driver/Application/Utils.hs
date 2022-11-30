module Wilde.Driver.Application.Utils where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.String as S

import qualified Network.HTTP.Types as HttpTypes

import Wilde.Driver.Application.Types


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


singleResponseHeaders :: MimeType
                      -> [HttpTypes.Header]
singleResponseHeaders mimeType = [(HttpTypes.hContentType, S.fromString mimeType)]

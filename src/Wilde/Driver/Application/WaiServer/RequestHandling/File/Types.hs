module Wilde.Driver.Application.WaiServer.RequestHandling.File.Types
(
    module Wilde.Driver.Application.Web.Types,
    MimeTypeMapping,
) where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Text (Text)
import Data.Map.Strict (Map)

import Wilde.Driver.Application.Web.Types


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------



-- | A mapping: file extension -> mime type
type MimeTypeMapping = Map Text MimeType

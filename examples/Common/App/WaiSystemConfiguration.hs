module Common.App.WaiSystemConfiguration where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Blaze.ByteString.Builder.Char8 as BChar8
import qualified Data.ByteString.Char8 as Char8

import Wilde.Driver.Application.Wai.Application


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


systemConfiguration :: SystemConfiguration
systemConfiguration = SystemConfiguration
                      {
                        contentEncoder  = BChar8.fromString
                      , queryVarDecoder = Char8.unpack
                      }

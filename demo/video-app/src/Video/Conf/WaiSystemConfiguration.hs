module Video.Conf.WaiSystemConfiguration where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------

import qualified Data.Text.Encoding as TE

import qualified Blaze.ByteString.Builder.Char8 as BChar8
import qualified Data.ByteString.Char8 as Char8

import Wilde.Driver.Application.Cgi.Wai


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


codingConfiguration :: CodingConfiguration
codingConfiguration = CodingConfiguration
  {
    contentEncoder  = BChar8.fromString
  , queryVarDecoder = Char8.unpack
  , queryTDecoder   = TE.decodeUtf8
  , contentTEncoder = TE.encodeUtf8Builder
  }

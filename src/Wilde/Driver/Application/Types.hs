module Wilde.Driver.Application.Types where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Blaze.ByteString.Builder as B
import           Control.Monad.Trans.Except (ExceptT, throwE)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type ErrorMessage = Text

newtype HandlingError = HandlingError ErrorMessage deriving Show

type HandlerResolvingMonad a = ExceptT HandlingError IO a

throwRequestHandlerException :: Text -> HandlerResolvingMonad a
throwRequestHandlerException = throwE . HandlingError

throwBadRequest :: Text -> HandlerResolvingMonad a
throwBadRequest = throwRequestHandlerException

-- | The (parsed) path of a Wai request.
type RequestPath = [Text]

type MimeType = Text

type ContentEncoder = String -> B.Builder

type ContentDecoder = BS.ByteString -> String

type TextEncoder = Text -> BSB.Builder
type TextDecoder = BS.ByteString -> Text

data CodingConfiguration =
  CodingConfiguration
  {
    contentEncoder  :: ContentEncoder
  , queryVarDecoder :: ContentDecoder
  , queryTDecoder   :: TextDecoder
  , contentTEncoder :: TextEncoder
  }

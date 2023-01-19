module Wilde.Driver.Application.WaiServer.RequestHandling.Types where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.Map.Strict as M
import qualified Network.Wai as Wai

import           Wilde.Driver.Application.Types (HandlerResolvingMonad, HandlingError, RequestPath)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type WaiResponder = Wai.Response -> IO Wai.ResponseReceived

type ErrorHandler = HandlingError -> IO Wai.ResponseReceived

-- | Handler of a request that has been validated, given an error handler and
-- a method for responing.
type ValidRequestHandler = ErrorHandler -> WaiResponder -> IO Wai.ResponseReceived

{- | Maps a request to a request handler.

Errors can be reported so that an invalid request can raise an error
without "serving" the request.

It can be difficult/expensive/impossible to check all properties of
a request to garrantee that is totally valid.
E.g. checking that a requested object exists in a database might need
execute the "service", or would be very expensive.
It is up to the implementation to decide what is checked and reported
as an exception in this monad, and what is handled/detected inside the
request handler.
-}
type RequestHandlerResolver = Wai.Request -> HandlerResolvingMonad ValidRequestHandler

-- | The request paths that are handled as files
-- (each together with its corresponding file system path).
type FilePaths = M.Map RequestPath FilePath

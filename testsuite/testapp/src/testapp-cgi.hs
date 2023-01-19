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

module Main where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Blaze.ByteString.Builder.Char8 as BChar8
import qualified Data.ByteString.Char8 as Char8

import qualified Data.Text.Encoding as TE

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.CGI as WaiCGI

import           Database.HDBC.MariaDB as Db

import qualified Wilde.Application.ApplicationConfiguration as AppConf
import           Wilde.Driver.Application.Cgi.Wai
import qualified Wilde.Utils.Logging.Class as Logger

import           ApplicationConfiguration


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


systemConfiguration :: CodingConfiguration
systemConfiguration = CodingConfiguration
  {
    contentEncoder  = BChar8.fromString
  , queryVarDecoder = Char8.unpack
                      -- , queryVarDecoder = UTF8.toString
  , queryTDecoder   = TE.decodeUtf8
  , contentTEncoder = TE.encodeUtf8Builder
  }

waiApp :: Wai.Application
waiApp = newApplication systemConfiguration appConf

appConf :: AppConf.ApplicationConfiguration
appConf = newAppConfig $ newLogger Logger.LIBRARY

main :: IO ()
main = Db.withRTSSignalsBlocked $ WaiCGI.run waiApp

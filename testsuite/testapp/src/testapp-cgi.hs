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
import qualified Data.ByteString.UTF8 as UTF8

import Wilde.Driver.Application.Wai.Application

import ApplicationConfiguration

import Database.HDBC.MySQL

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.CGI as WaiCGI


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


systemConfiguration :: SystemConfiguration
systemConfiguration = SystemConfiguration
                      {
                        contentEncoder  = BChar8.fromString
                      , queryVarDecoder = Char8.unpack
                      -- , queryVarDecoder = UTF8.toString
                      }

waiApp :: Wai.Application
waiApp = csApplication systemConfiguration appConfig

main :: IO ()
main = withRTSSignalsBlocked $ WaiCGI.run waiApp

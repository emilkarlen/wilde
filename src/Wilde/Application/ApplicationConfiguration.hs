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

-- | Configuration of an appliation that is independent of the
-- Driver.
module Wilde.Application.ApplicationConfiguration
       (
         module Wilde.Application.ApplicationServices,

         ApplicationConfiguration(..),
         
         Translations.Translations(..),

         DbConf.Configuration(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Media.Database.Configuration as DbConf
import qualified Wilde.Media.Translations as Translations

import Wilde.Application.ApplicationServices
import Wilde.Application.StandardServices
import qualified Wilde.Media.Presentation as Presentation
import Wilde.Application.StandardServiceLinks

-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Configuration of an application
-------------------------------------------------------------------------------


data ApplicationConfiguration =
     ApplicationConfiguration
     {
       appServices                      :: ApplicationServices
     , translations                     :: Translations.Translations
     , dbConfiguration                  :: DbConf.Configuration
     , standardServiceLinkRenderer      :: StandardServiceLinkRenderer
     , getStdObjectTypeServiceRenderer  :: Presentation.Monad ObjectTypeServiceLinkRenderer
     , getStdObjectServiceRenderer      :: Presentation.Monad ObjectServiceLinkRenderer
     , getGenericServiceLinkRenderer    :: Presentation.Monad
                                           Presentation.GenericServiceLinkRenderer
     , appCssFile                       :: Maybe String
     }

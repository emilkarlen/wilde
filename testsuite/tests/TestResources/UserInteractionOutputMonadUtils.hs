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

-- | Test utilities related to "Wilde.Media.UserInteraction.Output".
module TestResources.UserInteractionOutputMonadUtils
       (
         -- * Test environments

         emptyEnv,
         withMedia,
         check,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit.Base (Assertion)

import Wilde.Media.UserInteraction.Output
import qualified Wilde.Media.Database.Configuration as DbConf
import qualified Wilde.Utils.Logging.Class as Logger

import qualified Wilde.Media.ElementSet as ES

import qualified Wilde.Driver.UserInteraction.Translation.En as Tr
import qualified Wilde.Driver.UserInteraction.StandardServiceLinkRenderer as StandardServiceLinkRenderer

import qualified Wilde.Driver.Application.Cgi.ServiceLinkRenderers as CgiDriver

import qualified TestResources.Environment as Env

-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | An empty environment.
--
-- Especially, getting a database connection results in an error.
emptyEnv :: UserInteractionOutputEnvironment
emptyEnv =
  newEnvironment ES.empty ES.empty Env.emptyDbConfig emptyOutputing Env.emptyLogging

emptyOutputing :: Outputing
emptyOutputing =
  Outputing
  {
    outTranslations                  = Tr.translations
  , outStandardServiceLinkRenderer   = StandardServiceLinkRenderer.renderer
  , outMkStdObjectTypeServiceLink    = CgiDriver.getMkStandardObjectTypeServiceLink
  , outMkStdObjectServiceLink        = CgiDriver.getMkStandardObjectServiceLink
  , outgetMkGenericServiceLink = CgiDriver.getMkGenericServiceLink
  }

-------------------------------------------------------------------------------
-- | Assigns media to an environment.
--
-- Possible use:
--
-- > emptyEnv `withMedia` myMedia
-------------------------------------------------------------------------------
withMedia :: UserInteractionOutputEnvironment
          -> ES.ElementSet
          -> UserInteractionOutputEnvironment
withMedia env media = env { envMedia = media }

check :: UserInteractionOutputEnvironment
      -> (UserInteractionOutputResult a -> Assertion)
      -> UserInteractionOutputMonad a
      -> Assertion
check env assertion action = run env action >>= assertion

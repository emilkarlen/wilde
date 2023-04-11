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

import           Wilde.Media.UserInteraction.Output
import qualified Wilde.Media.Database.Configuration as DbConf
import qualified Wilde.Media.Presentation as Presentation

import qualified Wilde.Utils.Logging.Class as Logger

import qualified Wilde.Media.ElementSet as ES

import qualified Wilde.Driver.UserInteraction.Translation.En as Tr
import qualified Wilde.Driver.UserInteraction.StandardServiceLinkRenderer as StandardServiceLinkRenderer

import qualified Wilde.Driver.Application.Cgi.ServiceLinkRenderers as CgiDriver

import qualified TestResources.Environment as Env
import           TestResources.PresentationMonadUtils (emptyOutputing)

-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | An empty environment.
--
-- Especially, getting a database connection results in an error.
emptyEnv :: UserInteractionOutputEnvironment
emptyEnv =
  newEnvironment ES.empty ES.empty Env.emptyDbConfig emptyOutputing Env.emptyLogging


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
      -> UiO.Monad a
      -> Assertion
check env assertion action = run env action >>= assertion

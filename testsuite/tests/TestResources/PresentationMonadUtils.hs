-- | Test utilities related to "Wilde.Media.UserInteraction.Output".
module TestResources.PresentationMonadUtils
       (
         -- * Test environments

         emptyEnv,
         emptyOutputing,

         withCustomEnv,
         check,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Test.HUnit.Base (Assertion)

import qualified Wilde.Media.Presentation as Presentation
import           Wilde.Media.UserInteraction.Output

import qualified Wilde.Media.ElementSet as ES

import qualified Wilde.Driver.UserInteraction.Translation.En as Tr
import qualified Wilde.Driver.UserInteraction.StandardServiceLinkRenderer as StandardServiceLinkRenderer

import qualified Wilde.Driver.Application.Cgi.ServiceLinkRenderers as CgiServiceLinkRenderers
import qualified TestResources.Environment as Env


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | An empty environment.
--
-- Especially, getting a database connection results in an error.
emptyEnv :: Presentation.Environment
emptyEnv =
  Presentation.newEnvironment ES.empty Env.emptyDbConfig emptyOutputing Env.emptyLogging


emptyOutputing :: Outputing
emptyOutputing =
  Outputing
  {
    outTranslations                = Tr.translations
  , outServiceLinks =
    Presentation.ServiceLinks
    {
      Presentation.standardServiceLinkRenderer = StandardServiceLinkRenderer.renderer
    , Presentation.mkStdObjectTypeServiceLink  = CgiServiceLinkRenderers.getMkStandardObjectTypeServiceLink
    , Presentation.mkStdObjectServiceLink      = CgiServiceLinkRenderers.getMkStandardObjectServiceLink
    , Presentation.mkGenericServiceLink        = CgiServiceLinkRenderers.getMkGenericServiceLink
    }
  }

-------------------------------------------------------------------------------
-- | Assigns a Custom Environment to an environment.
-------------------------------------------------------------------------------
withCustomEnv :: Presentation.Environment
              -> ES.ElementSet
              -> Presentation.Environment
withCustomEnv env media = env { Presentation.envCustomEnvironment = media }

check :: Presentation.Environment
      -> (UserInteractionOutputResult a -> Assertion)
      -> Presentation.Monad a
      -> Assertion
check env assertion m = Presentation.run env m >>= assertion

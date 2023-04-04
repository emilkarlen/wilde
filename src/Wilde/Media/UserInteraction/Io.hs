module Wilde.Media.UserInteraction.Io
       (
         module Wilde.Media.UserInteraction,
         module UiI,
         module UiO,

         -- * IO

         UserInteractionIo(..),
         )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Media.UserInteraction
import qualified Wilde.Media.UserInteraction.Input as UiI
import qualified Wilde.Media.UserInteraction.Output as UiO


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - IO -
-------------------------------------------------------------------------------


-- | Input and output.
data UserInteractionIo defaultTypeForOutput a =
  UserInteractionIo
  {
    uiOutputer :: AttributeName -> UiO.WidgetConstructorGetter defaultTypeForOutput,
    uiInputer  :: AttributeName -> UiI.UserInteractionInputer a
  }

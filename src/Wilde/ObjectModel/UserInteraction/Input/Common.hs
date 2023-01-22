-- | Functionallity that is common to output for create.
module Wilde.ObjectModel.UserInteraction.Input.Common
       (
         inputer_fixedFromEnvHasPrecedence,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Media.ElementSet as ES
import qualified Wilde.Media.UserInteraction.Input as UiI
import qualified Wilde.Media.GenericStringRep as Gsr

import Wilde.ObjectModel.UserInteraction.Common (inputFixedFromEnv)
import Wilde.ObjectModel.ObjectModel


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inputs a value from User Interaction Input media.
--
-- Gives a fixed value if one is present in the media
-- (see 'inputFixedFromEnv').
--
-- Otherwise, the value is input via the widget.
-------------------------------------------------------------------------------
inputer_fixedFromEnvHasPrecedence :: Gsr.GenericStringRepInputer a
                                  -- ^ GSR inputer.
                                  -> (AttributeName -> UiI.UserInteractionInputer (ES.ElementInputResult a))
                                  -- ^ Widget inputer.
                                  -> AttributeName
                                  -> UiI.ObjectName
                                  -> UiI.Monad (ES.ElementInputResult a)
inputer_fixedFromEnvHasPrecedence gsrInputer widgetInputer attributeName objectName =
  do
    mbGsrForFixed <- inputFixedFromEnv attributeName objectName
    maybe
      (widgetInputer attributeName objectName)
      (parseGsr gsrInputer)
      mbGsrForFixed
  where
    elementKeyForAttr = ES.elementKey objectName attributeName

    parseGsr :: Gsr.GenericStringRepInputer a
             -> Gsr.GenericStringRep
             -> UiI.Monad (ES.ElementInputResult a)
    parseGsr inputer gsrString =
      pure $
      either
      (\attributeTypeError -> Left $ (elementKeyForAttr,attributeTypeError,Just gsrString))
      Right
      (inputer gsrString)

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
      return $
      either
      (\attributeTypeError -> Left $ (elementKeyForAttr,attributeTypeError,Just gsrString))
      Right
      (inputer gsrString)

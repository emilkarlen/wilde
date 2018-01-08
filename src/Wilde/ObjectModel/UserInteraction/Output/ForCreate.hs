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

-- | Construction of outputers for User Interaction.
module Wilde.ObjectModel.UserInteraction.Output.ForCreate
       (
         CreateCommon.AttributeTypeInfo(..),
         CreateCommon.ATTRIBUTE_OUTPUT_FOR_CREATE(..),

         CreateCommon.AttributeWidgetDefaultValueForCreate(..),
         CreateCommon.AttributeTypeOutputerForCreate,

         outputerForStdSetup,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Utils.AnyValue2 as AnyValue2

import Wilde.Media.UserInteraction.Output as UiO

import Wilde.ObjectModel.ObjectModelUtils as OmUtils

import qualified Wilde.ObjectModel.UserInteraction as OmUi
import qualified Wilde.ObjectModel.UserInteraction.Output.CreateCommon as CreateCommon
import qualified Wilde.ObjectModel.UserInteraction.Output.Common as OutputCommon


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Outputer constructors -
-------------------------------------------------------------------------------


-- | Constructs an outputer for 'AttributeType's from a single 'ObjectType'.
outputerForStdSetup :: OmUi.ATTRIBUTE_OUTPUT_FOR_CREATE atConf
                    => [Any (AttributeType atConf dbTable)]
                    -- ^ The attributes that should be input via the form,
                    -- and the order they should be displayed in it.
                    -> UiO.ObjectName
                    -> UiO.UserInteractionOutputMonad UiO.FormBlock
outputerForStdSetup = OutputCommon.outputerForSetupConstructor mkAtSetup
  where
    mkAtSetup :: OmUi.ATTRIBUTE_OUTPUT_FOR_CREATE atConf
              => Any (AttributeType atConf dbTable)
              -> AnyValue2.Container OutputCommon.AttributeTypeSetup
    mkAtSetup (Any at) = AnyValue2.Container
                                (CreateCommon.mkAttributeTypeSetup ati)
      where
        ati = CreateCommon.at2ati at

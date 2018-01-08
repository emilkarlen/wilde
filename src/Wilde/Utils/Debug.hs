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

-- | Utilities for debugging applications.
module Wilde.Utils.Debug 
       (
         debugFormBlock,
         debugService,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Media.WildeStyleType

import Wilde.Media.UserInteraction

import Wilde.Render.UserInteractionRendering

import Wilde.ApplicationConstruction.Service.ServiceTools

import qualified Wilde.WildeUi.StdValueTypes as StdValueTypes


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | A 'FormBlock' that displays a given string.
debugFormBlock :: [(String,String)] -> FormBlock
debugFormBlock debugInfos = FormBlock (map mkDisplay debugInfos) []
  where
    ek = globalElementKey "ek"
    mkDisplay = \(title,info) -> presentationOutputFormBlockRow (title,
                                                                 StdValueTypes.unquotedStringSvalue info)

-- | A 'Service' that displays a given string.
debugService :: [(String,String)] -> Service
debugService debugInfos =
   do
    component <- toServiceMonad $ formComponent form
    pageOkResult (withNeutralWildeStyle "Debug",
                  [component])
  where
    formBlock = debugFormBlock debugInfos
    form     = formForBlock formBlock Nothing

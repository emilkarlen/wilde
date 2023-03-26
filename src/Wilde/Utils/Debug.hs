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


import Wilde.WildeUi.WildeStyleType

import Wilde.Media.UserInteraction

import Wilde.Render.UserInteractionRendering ( formComponent )

import Wilde.ApplicationConstruction.Service.ServiceTools

import qualified Wilde.WildeUi.StdValueTypes as StdValueTypes

import Wilde.Application.Service.Service


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

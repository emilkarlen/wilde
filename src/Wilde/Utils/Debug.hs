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


import           Wilde.WildeUi.WildeStyle

import           Wilde.Media.UserInteraction

import           Wilde.ApplicationConstruction.Service.ServiceTools
import qualified Wilde.ApplicationConstruction.UserInteraction.Output.FormComponent as FormComponent

import qualified Wilde.WildeUi.StdValueTypes as StdValueTypes

import           Wilde.Application.Service.Service


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | A 'FormBlock' that displays a given string.
debugFormBlock :: [(String,String)] -> FormBlock
debugFormBlock debugInfos = formBlock_neutral (map mkDisplay debugInfos) []
  where
    ek = globalElementKey "ek"
    mkDisplay (title,info) =
      presentationOutputFormBlockRow (title, StdValueTypes.unquotedStringSvalue info)

-- | A 'Service' that displays a given string.
debugService :: [(String,String)] -> Service
debugService debugInfos =
   do
    component <- toServiceMonad $ FormComponent.getFormComponent form
    pageOkResult (withNeutralWildeStyle "Debug",
                  [component])
  where
    formBlock = debugFormBlock debugInfos
    form     = formForBlock formBlock Nothing

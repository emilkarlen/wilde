-- | English translations.
module Wilde.Driver.UserInteraction.Translation.En
       (
         AppConf.Translations(..),
         translations,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Application.Service.PopUp

import qualified Wilde.Application.ApplicationConfiguration as AppConf


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


translations :: AppConf.Translations
translations =
  AppConf.Translations
  {
    AppConf.trContinueQuestion         = "Continue?"
  , AppConf.trThisIsImpossibleToUndo   = "This cannot be undone."
  , AppConf.trIformationDialogTitle    = "Information"
  , AppConf.trErorPageTitle            = "Error"
  , AppConf.trServiceNotSpecifiedError = "Service not specified"
  , AppConf.trUnknownServiceError      = "Unknown service"
  , AppConf.trButtonTexter             = buttonTexter
  }

buttonTexter :: Button -> String
buttonTexter btn =
  case btn of
    Ok    -> "OK"
    Yes   -> "Yes"
    No    -> "No"
    Reset -> "Reset"

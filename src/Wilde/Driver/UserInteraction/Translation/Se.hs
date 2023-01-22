-- | Swedish translations.
module Wilde.Driver.UserInteraction.Translation.Se
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
    AppConf.trContinueQuestion         = "Fortsätta?"
  , AppConf.trThisIsImpossibleToUndo   = "Detta går inte att ångra."
  , AppConf.trIformationDialogTitle    = "Information"
  , AppConf.trErorPageTitle            = "Fel"
  , AppConf.trServiceNotSpecifiedError = "Ingen tjänst har angivits"
  , AppConf.trUnknownServiceError      = "Okänd tjänst"
  , AppConf.trButtonTexter             = buttonTexter
  }

buttonTexter :: Button -> String
buttonTexter btn =
  case btn of
    Ok    -> "OK"
    Yes   -> "Ja"
    No    -> "Nej"
    Reset -> "Återställ"

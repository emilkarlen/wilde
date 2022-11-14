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

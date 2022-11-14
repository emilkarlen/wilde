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

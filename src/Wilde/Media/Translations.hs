-- | Translations of texts needed by every application.
module Wilde.Media.Translations
       (
         Translations(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Application.Service.PopUp as PopUp


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Translation of some texts needed by this main method.
data Translations =
  Translations
  {
    trContinueQuestion         :: String
  , trThisIsImpossibleToUndo   :: String
  , trIformationDialogTitle    :: String
  , trErorPageTitle            :: String
  , trServiceNotSpecifiedError :: String
  , trUnknownServiceError      :: String
  , trButtonTexter             :: PopUp.Button -> String
  }

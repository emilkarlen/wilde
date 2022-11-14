{-# LANGUAGE TypeSynonymInstances #-}
module Wilde.Service.Error where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Utils.NonEmptyList as NonEmpty
import qualified Wilde.Media.ElementSet as ES
import           Wilde.Media.WildeMedia as WM
import qualified Wilde.Media.Database as DbM
import qualified Wilde.Media.UserInteraction.Output as UiOM
import qualified Wilde.Media.UserInteraction.Input as UiI


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data ServiceError = SInvocationError    InvocationError
                  | DbIoError           DbM.DatabaseError
                  | UiMediaLookupError  ES.ElementLookupError
                  | UiObjectInputError  (NonEmpty.List ObjectInputErrorInfo)
                  | UnclassifiedError   WM.UnclassifiedError
                  | NormalError         String
                  | SObjectModelError   String
                  | ImplementationError String
                    deriving Show

data InvocationError = MandatoryVariableMissing String (Maybe [String]) -- ^ Var Name,actual value
                     | ValueSyntax String String                        -- ^ Var Name,actual value
                     | ValueValue  String String                        -- ^ Var Name,actual value
                       deriving Show

-- | Class for the error "sub types" of ServiceError.
class ToServiceError a where
  toServiceError :: a -> ServiceError

instance ToServiceError GeneralError where
  toServiceError (GeneralUnclassifiedError s) = UnclassifiedError (unclassifiedError s)
  toServiceError (GeneralObjectModelError  s) = SObjectModelError s

instance ToServiceError ObjectToNativeError where
  toServiceError = SObjectModelError . show

instance ToServiceError UnclassifiedError where
  toServiceError = UnclassifiedError

instance ToServiceError InvocationError where
  toServiceError = SInvocationError

instance ToServiceError DbM.TranslationError where
  toServiceError = DbIoError . DbM.DbTranslationError

instance ToServiceError DbM.DatabaseError where
  toServiceError = DbIoError

instance ToServiceError UiOM.UserInteractionOutputError where
  toServiceError (UiOM.DatabaseError error)        = toServiceError error
  toServiceError (UiOM.ObjectModelError string)    = SObjectModelError string
  toServiceError (UiOM.ImplementationError string) = ImplementationError string
  toServiceError (UiOM.MediaLookupError info)      = UiMediaLookupError info
  toServiceError (UiOM.UnclassifiedError error)    = toServiceError error

instance ToServiceError ServiceError where
  toServiceError = id

instance ToServiceError UiI.Error where
  toServiceError (UiI.ImplementationError string)   = ImplementationError string
  toServiceError (UiI.UnclassifiedError   err)      = UnclassifiedError err
  toServiceError (UiI.MediaLookupError info)        = UiMediaLookupError info

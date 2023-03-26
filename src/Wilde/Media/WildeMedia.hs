{-# LANGUAGE ExistentialQuantification #-}
-- | \"Media\" related to the concepts of Wilde.
module Wilde.Media.WildeMedia
       (
         -- Presentation

         PresentationOutput,
         PresentationOutputer,

         -- User Interaction

         -- * Type Errors

         ObjectModelTypeErrorCause(..),
         ObjectModelTypeOperation(..),
         ObjectModelTypeError(..),
         ObjectInputErrorInfo(..),

         ObjectToNativeError(..),
         ObjectAndObjectTypeMismatchError(..),

         anonOTN,
         anonOOT,

         -- ObjectToNativeError(..),
         CrossRefIdentifier(..),

         UnclassifiedError,
         unclassifiedError,

         GeneralError(..),
         GeneralResult,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.Typeable
import qualified Data.List.NonEmpty as NonEmpty

import           Wilde.Media.ElementSet
import           Wilde.WildeUi.WildeValue
import           Wilde.Utils.Utils
-- import           Wilde.WildeUi.UiPrimitives


-------------------------------------------------------------------------------
-- - Database -
-------------------------------------------------------------------------------


-- | The kind of error that can appear when inputing a value from the database.
-- type DatabaseInputError = String

-- | Type for reading from database.
-- type DatabaseInputResult a = Either DatabaseInputError a

-- class ToTranslationError err where
--   toTranslationError :: err -> TranslationError

-- instance ToTranslationError ConvertError where
--   toTranslationError err = AttributeTranslationError "" err


-------------------------------------------------------------------------------
-- - Presentation -
-------------------------------------------------------------------------------


type PresentationOutput = AnySVALUE

type PresentationOutputer a = a -> PresentationOutput


-------------------------------------------------------------------------------
-- - User Interaction -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Other -
-------------------------------------------------------------------------------


-- | Information about an error related to the input of an Object from
-- User Interaction Media.
data ObjectInputErrorInfo =
  ObjectInputErrorInfo
  {
    otKey      :: CrossRefIdentifier
  , oName      :: String
  , attrErrors :: NonEmpty.NonEmpty ElementLookupError
  }
  deriving Show

-- | The type of error that can occurr when translating a "general" object
-- (an 'Object') to it's "native" type, using 'otToNative'.
-- data ObjectToNativeError

data ObjectModelTypeErrorCause
     = NumAttributesError (Mismatch Int)
     | AttributeTypeError (Mismatch TypeRep)
     deriving Show

data ObjectModelTypeOperation
     = ObjectToNative           -- ^ Translation of obj to it's "native" type.
     | ObjectAndObjectTypeMatch -- ^ Mismatch of types of an 'ObjectType' and an 'Object
                                -- that should be an instance of the 'ObjectType'.
       deriving Show

-- | Error type when the operation is 'ObjectToNative'.
data ObjectToNativeError = ObjectToNativeError
                           {
                             otneDescription :: String,
                             otneCause       :: ObjectModelTypeErrorCause
                           }
                           deriving Show

anonOTN :: ObjectModelTypeErrorCause -> ObjectToNativeError
anonOTN cause = ObjectToNativeError "" cause

-- | Error type when the operation is 'ObjectAndObjectTypeMismatch'.
data ObjectAndObjectTypeMismatchError = ObjectAndObjectTypeMismatchError
                           {
                             ootDescription :: String,
                             ootCause       :: ObjectModelTypeErrorCause
                           }
                           deriving Show

anonOOT :: ObjectModelTypeErrorCause -> ObjectAndObjectTypeMismatchError
anonOOT cause = ObjectAndObjectTypeMismatchError "" cause


-- | An error that has to do with the types of objects and attributes,
-- and that stems from the application implementation.
data ObjectModelTypeError = ObjectModelTypeError
                            {
                              operation   :: ObjectModelTypeOperation,
                              cause       :: ObjectModelTypeErrorCause,
                              description :: String
                             }
                            deriving Show


-------------------------------------------------------------------------------
-- - Cross References -
-------------------------------------------------------------------------------

-- | A String that can serve as an identifier in most media.
--
-- Typically allowed: a-z,A-Z,0-9,_,-
type CrossRefIdentifier = String

-- | Type of errors that are not related to database communication.
-- May be refined in the future, if more structure is needed.
newtype UnclassifiedError = UnclassifiedError String
                            deriving Show

-- "Constructor" for 'UnclassifiedError'.
unclassifiedError :: String -> UnclassifiedError
unclassifiedError = UnclassifiedError

data GeneralError = GeneralUnclassifiedError String
                  | GeneralObjectModelError String

type GeneralResult a = Either GeneralError a

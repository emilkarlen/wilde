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

{-# LANGUAGE ExistentialQuantification #-}
-- | \"Media\" related to the concepts of Wilde.
module Wilde.Media.WildeMedia
       (
         -- Presentation
         Title(..),
         
         StyledTitle,
         neutralTitle,
         
         PresentationOutput,
         PresentationOutputer,
         
         module Wilde.WildeUi.UiPrimitives,
         
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


import Data.Typeable

import qualified Wilde.Utils.NonEmptyList as NonEmpty
import Wilde.Utils.Utils

import Wilde.Media.ElementSet

import Wilde.Media.WildeValue

import Wilde.WildeUi.UiPrimitives


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


type Title = String

type StyledTitle = WildeStyling Title

neutralTitle :: Title -> StyledTitle
neutralTitle = withNeutralWildeStyle

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
  , oName      :: Title
  , attrErrors :: NonEmpty.List ElementLookupError
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

module Wilde.Media.Database.Error where


import Data.Convertible.Base
import Wilde.Media.Database
import Wilde.Media.WildeMedia (ObjectToNativeError)

class ToDatabaseError a where
  toDatabaseError :: a -> DatabaseError

instance ToDatabaseError TranslationError where
  toDatabaseError = DbTranslationError

instance ToDatabaseError DatabaseError where
  toDatabaseError = id

instance ToDatabaseError ConvertError where
  toDatabaseError = DbTranslationError . AttributeTranslationError ""

instance ToDatabaseError ObjectToNativeError where
  toDatabaseError = DbTranslationError . ObjectToNativeInDbError

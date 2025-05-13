{-# LANGUAGE MultiParamTypeClasses #-}

-- | Monad for the translation to/from the internal represenation
-- and the database representation.
module Wilde.Media.Database
       (
         DatabaseOutput,
         DatabaseInput,
         DatabaseOutputer,
         DatabaseInputer,
         DatabaseIo(..),
         DatabaseColumn(..),
         DatabaseTable(..),
         TranslationError(..),
         DatabaseError(..),

         TranslationMonad(..),
         runTranslation,
         doFail,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Control.Monad.Error.Class as ME

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

import Data.Convertible.Base

import Database.HDBC.Types

import Wilde.Utils.Utils

import Wilde.Database.Sql

import qualified Wilde.Media.ElementSet as ES
import Wilde.Media.CustomEnvironment
import Wilde.Media.WildeMedia (ObjectToNativeError)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Media for outputing an attribute TO the database.
type DatabaseOutput = [SqlValue]

-- | Media for inputing an attribute FROM the database.
type DatabaseInput  = [SqlValue]

type DatabaseOutputer a = a -> ConvertResult DatabaseOutput

type DatabaseInputer  a = DatabaseInput -> ConvertResult a

-- | Methods for transforming a value to and from the Database Media.
--
-- | A 'DatabaseOutputer' and a 'DatabaseInputer'.
data DatabaseIo a = DatabaseIo
                  {
                    dbOutputer :: DatabaseOutputer a,
                    dbInputer  :: DatabaseInputer  a
                  }

newtype DatabaseColumn dbTable =
  DatabaseColumn
  {
    columnName :: dbTable
  }
  deriving Show

instance Functor DatabaseColumn where
  fmap f (DatabaseColumn n) = DatabaseColumn (f n)

newtype DatabaseTable =
  DatabaseTable
  {
    tableName :: SqlIdentifier
  }

-- | The type of errors that can occurr when translating info from/to SQL.
--
-- (Must be located in this module because it is used by the
-- User Interaction Monad.)
data TranslationError = UnknownTranslationError String
                      | RecordTranslationError String (Mismatch String)
                      | AttributeTranslationError String ConvertError
                      | ObjectModelError String
                      | ObjectToNativeInDbError ObjectToNativeError
                      | ImplementationTranslationError String
                      deriving Show

data DatabaseError = DbTranslationError TranslationError
                   | DbNoRows      String (Maybe (Mismatch Int))
                   | DbTooManyRows String (Maybe (Mismatch Int))
                   | DbUnclassifiedError String
                     deriving Show

newtype TranslationMonad a = TranslationMonad (ExceptT TranslationError (Reader ES.ElementSet) a)

instance ME.MonadError TranslationError TranslationMonad where
  throwError e = TranslationMonad $ throwE e
  catchError (TranslationMonad m) handler = TranslationMonad $ catchE m handler'
    where
      handler' e =
        let (TranslationMonad m) = handler e
        in  m

doFail :: TranslationError -> TranslationMonad a
doFail = ME.throwError

instance Monad TranslationMonad where
  (TranslationMonad m) >>= f = TranslationMonad $
                           do a <- m
                              let TranslationMonad m' = f a
                              m'

instance Applicative TranslationMonad where
  pure = TranslationMonad . pure
  (TranslationMonad ma) <*> (TranslationMonad mb) = TranslationMonad $ ma <*> mb

instance Functor TranslationMonad where
  fmap f (TranslationMonad m) = TranslationMonad $ fmap f m

instance MonadWithCustomEnvironment TranslationMonad where
  getCustomEnvironment = TranslationMonad $ lift ask

runTranslation :: ES.ElementSet
               -> TranslationMonad a
               -> Either TranslationError a
runTranslation env (TranslationMonad m) = runReader (runExceptT m) env

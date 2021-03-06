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
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Control.Monad.Error.Class as ME

import Control.Monad.Trans
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader

import Data.Convertible.Base

import Database.HDBC.Types

import Wilde.Utils.Utils

import Wilde.Database.Sql

import qualified Wilde.Media.ElementSet as ES
import Wilde.Media.CustomEnvironment


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

data DatabaseColumn dbTable =
  DatabaseColumn
  {
    columnName :: dbTable
  }
  deriving Show

instance Functor DatabaseColumn where
  fmap f (DatabaseColumn n) = DatabaseColumn (f n)

data DatabaseTable =
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
                      | ImplementationTranslationError String
                      deriving Show

instance Error TranslationError where
  strMsg s = ImplementationTranslationError s

data DatabaseError = DbTranslationError TranslationError
                   | DbNoRows      String (Maybe (Mismatch Int))
                   | DbTooManyRows String (Maybe (Mismatch Int))
                   | DbUnclassifiedError String
                     deriving Show

instance Error DatabaseError where
  strMsg = DbUnclassifiedError

newtype TranslationMonad a = TranslationMonad (ErrorT TranslationError (Reader ES.ElementSet) a)

instance ME.MonadError TranslationError TranslationMonad where
  throwError e = TranslationMonad $ throwError e
  catchError (TranslationMonad m) handler = TranslationMonad $ catchError m handler'
    where
      handler' e =
        let (TranslationMonad m) = handler e
        in  m

instance Monad TranslationMonad where
  return = TranslationMonad . return
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
runTranslation env (TranslationMonad m) = runReader (runErrorT m) env

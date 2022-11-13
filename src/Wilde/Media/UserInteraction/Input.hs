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

-- | Monad for reading from User Interaction Input - reading from a User Interface.
module Wilde.Media.UserInteraction.Input
       (
         module Wilde.Media.UserInteraction,
         module Wilde.Media.MonadWithInputMedia,
         module ES,

         -- * Types
         
         UserInteractionInputer,
         
         -- * The Monad
         
         Monad,
         Environment(..),
         Result(..),
         run,
         throwErr,
         catchErr,

         -- ** Errors

         Error(..),

         -- ** Liftings and translations
         
         ToError(..),
         ToMonad(..),

         liftIOWithError,
         liftIOWithErrorT,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Prelude hiding (Monad)

import qualified Control.Monad as MMonad
import qualified Control.Monad.Trans as MTrans
import qualified Control.Monad.Trans.Except as MExcept
import qualified Control.Monad.Trans.Reader as MReader

import qualified Wilde.Media.ElementSet as ES
import Wilde.Media.CustomEnvironment
import Wilde.Media.UserInteraction
import Wilde.Media.WildeMedia

import Wilde.Media.MonadWithInputMedia


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type UserInteractionInputer a = ObjectName -> Monad a

-- | The kind of error that can appear when inputing a value from the User Interaction.
data Error = UnclassifiedError   UnclassifiedError
           | ImplementationError String
           | MediaLookupError    ES.ElementLookupError
           | ObjectInputError    ObjectInputErrorInfo
             deriving Show

-- | Type for reading from user interaction input.
type Result a = Either Error a

-- | Class for the error "sub types" of UserInteractionOutputError.
class ToError a where
  toError :: a -> Error

instance ToError Error where
  toError = id

instance ToError ObjectInputErrorInfo where
  toError = ObjectInputError

newtype Monad a =
  Monad (MExcept.ExceptT Error (MReader.ReaderT Environment IO) a)

data Environment =
  Environment
  {
    envInputMedia        :: ES.ElementSet
  , envCustomEnvironment :: ES.ElementSet
  }

instance MMonad.Monad Monad where
  return = Monad . return
  (Monad m) >>= f = Monad $
                    do a <- m
                       let Monad m' = f a
                       m'

instance Applicative Monad where
  pure = Monad . pure
  (Monad ma) <*> (Monad mb) = Monad $ ma <*> mb

instance Functor Monad where
  fmap f m = do a <- m; return (f a)

instance MonadWithInputMedia Monad where
  getInputMedia = Monad $ MTrans.lift $ MReader.asks envInputMedia
  
instance MonadWithInputMediaAndLookup Monad where
  inInputMedia = ES.integrateLookup integration
    where
      integration = ES.ElementSetMonadIntegration
        {
          ES.getElementSet = getInputMedia
        , ES.throwError    = throwElementLookupError
        }

instance MonadWithCustomEnvironment Monad where
  getCustomEnvironment = Monad $ MTrans.lift $ MReader.asks envCustomEnvironment
  
instance MonadWithCustomEnvironmentAndLookup Monad where
  inCustomEnvironment = ES.integrateLookup integration
    where
      integration = ES.ElementSetMonadIntegration
        {
          ES.getElementSet = getCustomEnvironment
        , ES.throwError    = throwElementLookupError
        }

throwElementLookupError :: ES.ElementLookupError -> Monad a
throwElementLookupError err = throwErr $ MediaLookupError err


-- | Runs a 'Monad'.
run :: Environment
    -> Monad a 
    -> IO (Either Error a)
run env (Monad m) =
  MReader.runReaderT (MExcept.runExceptT m) env

-- | Corresponds to 'Control.Monad.Trans.Except's throwE.
throwErr :: ToError err => err -> Monad a
throwErr err = Monad $ MExcept.throwE (toError err)

-- | Corresponds to 'Control.Monad.Trans.Except's catchE.
catchErr :: Monad a            -- ^ The computation that can throw an error.
         -> (Error -> Monad a) -- ^ Error handler
         -> Monad a
catchErr m handler =
  let
    (Monad errT) = m
    handlerErrT err = let (Monad errT) = handler err
                      in  errT
  in
   Monad $ MExcept.catchE errT handlerErrT

instance MTrans.MonadIO Monad where
  liftIO = Monad . MTrans.lift . MTrans.lift

class ToMonad m where
  toMonad :: m a -> Monad a

instance ToError err => ToMonad (Either err) where
  toMonad (Left err) = throwErr err
  toMonad (Right x)  = return x

-- | Integrates monads of type "IO (Either err a)"
-- into the Monad monad
liftIOWithError :: ToError err
                => IO (Either err a)
                -> Monad a
liftIOWithError io =
  do
    res <- MTrans.liftIO io
    case res of
      Left err -> throwErr err
      Right ok -> return ok

-- | Integrates monads of type "ExceptT err IO"
-- into the Monad monad
liftIOWithErrorT :: ToError err
                 => MExcept.ExceptT err IO a
                 -> Monad a
liftIOWithErrorT io =
  do
    res <- MTrans.liftIO $ MExcept.runExceptT io
    either throwErr return res

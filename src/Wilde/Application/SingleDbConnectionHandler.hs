-- | Tool for creating a single connection when it is first requested,
-- and resuing this connection in following requests.
module Wilde.Application.SingleDbConnectionHandler
(
    Handler(..),
    newHandler,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Control.Monad.ST
import Data.STRef
import Control.Monad

import qualified Database.HDBC as HDBC


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data Handler = Handler
  {
    getConnection      :: IO HDBC.ConnWrapper
  , disconnectIfNeeded :: IO String
  -- ^ Gives a message suitable for logging.
  }

{- | Creates a pair of actions for handling a single db connection.

- Get the connection

Creates the connection the first time it is requested.
Following requests gives the same connection.

For this to work, the connection must not be closed by the user!

- Close the connection

Closes the connection, if it has been created.
This should be invoked when a db connection is no more needed.
-}
newHandler :: IO HDBC.ConnWrapper -- ^ Gets a new db connection
           -> IO Handler
newHandler getNewDbConn =
    do
        stRef <- stToIO $ initialState getNewDbConn
        pure $ Handler (mkGetConnection stRef) (doCleanup stRef)

type Action = IO HDBC.ConnWrapper
type State = (Action, Int) -- ^ (get new or current connection, request count)

initialState :: Action -> ST RealWorld (STRef RealWorld State)
initialState getInitial = newSTRef (getInitial, 0)

mkGetConnection :: STRef RealWorld State -> Action
mkGetConnection stRef = do
  action  <- stToIO getCurrentActionWithCount
  ret_val <- action
  stToIO $ setAction $ pure ret_val
  pure ret_val

  where
    getCurrentActionWithCount :: ST RealWorld Action
    getCurrentActionWithCount = do
      (action, count) <- readSTRef stRef
      modifySTRef' stRef $ \(a,n) -> (a, n+1)
      pure action

    setAction :: Action -> ST RealWorld ()
    setAction action = do
      modifySTRef' stRef $ \(_,n) -> (action, n)

doCleanup :: STRef RealWorld State -> IO String
doCleanup stRef = do
  (action, numRequests) <- getState
  when (numRequests > 0) $ do
    conn <- action
    HDBC.disconnect conn
  pure $ report numRequests

  where
    getState :: IO State
    getState = stToIO $ readSTRef stRef

     
    report :: Int -> String
    report n = report_header n ++ report_disConnInfo n

    report_header :: Int -> String
    report_header n = "Num connection requests: " ++ show n ++ ". "

    report_disConnInfo :: Int -> String
    report_disConnInfo 0 = "No disconnect needed."
    report_disConnInfo _ = "Connection disconnected."
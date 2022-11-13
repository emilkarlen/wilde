module Wilde.ApplicationTool.Command
       (
         CommandEnv(..),
         Commands(..),
         Command(..),
         CommandWithParsedArgs(..),
         Service(..),
         
         withParsedArgs,
         getCar,
       )
       where

       
-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import System.Console.GetOpt

import Database.HDBC

import qualified Wilde.ApplicationTool.DbExecution as SqlExec
import Wilde.Database.BackEndDdl

import Wilde.ApplicationTool.FlagsAndOptions as FlagsAndOptions


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data CommandEnv om =
  CommandEnv
  {
    connectionProvider :: IO ConnWrapper
  , ddlRenderer        :: DdlRenderer
  , dmlRenderer        :: SqlExec.DmlRenderer
  , objectModel        :: om
  }

-- | The list of commands that a tool handles.
--
-- (names,(description,function)).
type Commands om = [(String,(String,Command om))]

type Command om = CommandEnv om -> [String] -> IO ()

type CommandWithParsedArgs om = CommandEnv om -> ([Flag], [String]) -> IO ()

data Service = GetAllPlain | GetOnePlain

getCar :: CommandEnv om
       -> IO SqlExec.ConnectionAndRenderer
getCar (CommandEnv { connectionProvider = cp, dmlRenderer = dmlr }) =
  do
    conn <- cp
    return $
      SqlExec.ConnectionAndRenderer
      {
        SqlExec.carConnection = conn
      , SqlExec.carRenderer   = dmlr
      }

withParsedArgs :: CommandWithParsedArgs om 
               -> CommandEnv om 
               -> [String] 
               -> IO ()
withParsedArgs cmd env args =
  case getOpt Permute options args of
    (o,n,[]  ) -> cmd env (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: COMMAND [OPTION...]"

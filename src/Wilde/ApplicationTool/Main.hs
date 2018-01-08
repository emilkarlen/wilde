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

-------------------------------------------------------------------------------
-- | The main method for the CLI Application Tool
-------------------------------------------------------------------------------
module Wilde.ApplicationTool.Main 
       (
         module Wilde.ApplicationTool.Command,
         
         cliToolMain,
       )
       where

import System.Environment

import System.Console.GetOpt

import Wilde.ApplicationTool.ErrorHandling

import qualified Wilde.ApplicationTool.FlagsAndOptions as FlagsAndOptions
import Wilde.ApplicationTool.Command


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


help :: Commands om -> Command om
help commands om args =
  do
    putStrLn "COMMANDS"
    mapM_ putStrLn (map fst commands)
    putStrLn $ usageInfo "OPTIONS" FlagsAndOptions.options

cliToolMain :: CommandEnv om
            -> Commands om
            -> IO ()
cliToolMain env commands =
  do
    args <- getArgs
    if null args
      then argsMissing
      else executeCommand
           env 
           (("help",("Help",help commands)) : commands) 
           (head args) 
           (tail args)
           
      where
        executeCommand :: CommandEnv om -> Commands om -> String -> [String] -> IO ()
        executeCommand env commands cmdName cmdArgs =
          case lookup cmdName commands of
            Nothing -> unknownCommand cmdName
            Just (cmdDescr,command) -> command env cmdArgs
        
        argsMissing        = msgFail "No command given (give argument \"help\" for help)."
        unknownCommand cmd = msgFail $ "Unknown command: " ++ cmd


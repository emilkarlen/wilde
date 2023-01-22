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

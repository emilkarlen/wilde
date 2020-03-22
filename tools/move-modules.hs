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

module Main where

-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Control.Monad

import Data.List
import Data.Char

import System.IO


import System.Console.GetOpt

import Control.Monad

import qualified Data.Set as Set

import Data.Maybe ( fromMaybe )
import System.FilePath
import System.Exit
import System.Cmd
import System.Environment
import System.IO
import System.IO.Temp

import IdentifierLexer


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


helpDescription :: String
helpDescription =
  "Primitive Haskell refactoring tool for moving modules.\n\
  \\n\
  \Does only modify _contents_ of source files. Does not move files!"

type PackageName = String
type ModuleName  = String

main = do
  (files,options) <- getProgramArguments
  processStdin options
  

-- | Reads (name,GHC-args,options)
getProgramArguments :: IO ([String],Options)
getProgramArguments =
  do
    args <- getArgs
    (options,additional) <- getOptionsAndArgs args
    when (not $ null additional) (
      do
        putStrLn usageHeader
        exitFailure
      )
    return (additional,options)

data Options =
  Options
  {
    optDestination    :: Maybe PackageName
  , optToMove         :: [ModuleName]
  , optDisplayHelp    :: Bool
  } deriving Show

defaultOptions =
  Options
  {
    optDestination    = Nothing
  , optToMove         = []
  , optDisplayHelp    = False
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [  Option ['f']     ["modules-from-file"]
    (ReqArg ((\ filePath opts -> parseAndAddExcludesFromFile filePath opts))
     "FILE")
    "Read module names to move from the given file."
  
  ,  Option ['m']     ["module"]
    (ReqArg ((\ moduleName opts -> return $ opts { optToMove = moduleName : (optToMove opts) }))
     "MODULE")
    "Specify a module to move. Repeat for moving many modules."
  
  , Option ['d']     ["destination"]
    (ReqArg ((\ pkg opts -> return $ opts { optDestination = Just pkg }))
     "PACKAGE")
    "Destination package. If not given, the destination is the top-level package."
  
  , Option ['h','?']     ["help"]
    (NoArg (\ opts -> return $ opts { optDisplayHelp = True }))
    "Display help and exit."
  ]

getOptionsAndArgs :: [String] -> IO (Options, [String])
getOptionsAndArgs argv =
       case getOpt RequireOrder options argv of
          (os,ns,[]  ) -> 
            do
              os' <- foldM (\soFar next -> next soFar) defaultOptions os
              when (optDisplayHelp os') $ displayHelpAndExit
              when (null (optToMove os')) $ displayErrAndExit ["No modules to move given (-m)"]
              return (os', ns)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usageHeader options))

displayErrAndExit :: [String] -> IO ()
displayErrAndExit errs = ioError (userError (concat errs ++ usageInfo usageHeader options))

displayHelpAndExit :: IO ()
displayHelpAndExit =
  do
    putStrLn helpDescription
    putStrLn ""
    putStr $ usageInfo usageHeader options
    exitSuccess



usageHeader = "Usage: [OPTION...] PROGRAM-FILE..."

parseAndAddExcludesFromFile :: FilePath -> Options -> IO Options
parseAndAddExcludesFromFile fileName options =
  do
    newModules <- readModuleNames
    let prevModules = optToMove options
    let nextModules = prevModules ++ newModules
    return $ options { optToMove = nextModules }

  where
    readModuleNames :: IO [String]
    readModuleNames = do
      contents <- readFile fileName
      return $ lexemes contents

processStdin :: Options -> IO ()
processStdin options =
  do
    contents <- getContents
    translate options contents 

processLine :: Options -> String -> String
processLine opts line =
  case stripPrefix "import " line of
    Nothing -> line
    (Just rest) -> case stripPrefix "qualified " rest of
      Nothing   -> "import "           ++ replace opts rest
      (Just r2) -> "import qualified " ++ replace opts r2
      
replace :: Options -> String -> String
replace options imp =
  let
    (moduleName,rest) = break isSpace imp
    moduleName'       = processIdentifier options moduleName
  in
   moduleName' ++ rest


readIdentifier :: String -> (String,String)
readIdentifier xs = span relevantIdentifierChar xs

processIdentifier :: Options -> ModuleName -> ModuleName
processIdentifier (Options {
                      optDestination = mbDest
                      , optToMove    = toMoves
                      })
                   moduleName =
                      if (moduleName `elem` toMoves)
                      then processMatch mbDest moduleName
                      else either
                           processQualified
                           id
                           (unqualify moduleName)
  where
    processQualified :: (String,String) -> String
    processQualified (qual,unquald) =
      if (qual `elem` toMoves)
      then processMatch mbDest qual ++ "." ++ unquald
      else qual ++ "." ++ unquald

processMatch :: Maybe String -> String -> String
processMatch mbDestinationPkg moduleName = newName
  where
    newName         = maybe
                      unqualifiedName
                      (\pkg -> pkg ++ ('.' : unqualifiedName))
                      mbDestinationPkg
    unqualifiedName = reverse $ takeWhile (/='.') $ reverse moduleName
  

unqualify :: String -> Either (String,String) String -- Either (Qualif,Unqualifd) Unqualifd
unqualify identifier =
  case break (=='.') (reverse identifier) of
    (s,[])    -> Right identifier
    (unq,_:q) -> Left (reverse q,reverse unq)

relevantIdentifierChar ch = isAlphaNum ch || ch == '.'

relevantIdentifierFirstChar = isAlpha


translate :: Options -> String -> IO ()
translate os []           = return ()
translate os ('-':'-':xs) = putStr "--" >> skipUntilNextLine xs            >>= translate os
translate os ('{':'-':xs) = putStr "{-" >> skipUntilDelimitedCommentEnd xs >>= translate os
translate os (x      :xs)
  | relevantIdentifierFirstChar x =
    let
      (identTail,rest) = readIdentifier xs
    in
     putStr (processIdentifier os (x:identTail)) >> translate os rest
  | otherwise = putChar x >> translate os xs

skipUntilNextLine :: String -> IO String
skipUntilNextLine [] = return []
skipUntilNextLine ('\n' : xs) = putChar '\n' >> return xs
skipUntilNextLine (x    : xs) = putChar x    >> skipUntilNextLine xs

skipUntilDelimitedCommentEnd :: String -> IO String
skipUntilDelimitedCommentEnd [] = return []
skipUntilDelimitedCommentEnd ('-':'}':xs) = putStr "-}" >> return xs
skipUntilDelimitedCommentEnd ('{':'-':xs) = putStr "{-" >>
                                            skipUntilDelimitedCommentEnd xs >>=
                                            skipUntilDelimitedCommentEnd
skipUntilDelimitedCommentEnd (x:xs)       = putChar x >> skipUntilDelimitedCommentEnd xs

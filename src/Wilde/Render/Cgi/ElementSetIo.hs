-- | Types and functionality for executing a Wilde Application
-- via a HTTP server.
--
-- TODO rename methods, type CGI -> ServerInput (or something)
module Wilde.Render.Cgi.ElementSetIo
       (

         -- * Reexporting

         Input(..),
         ServerVariables,
         ServerVariableElement,
         toMandatoryValue,
         toOptionalValue,

         -- * Conversion to/from 'ElementSet'

         inputFromCgiValues,
         inputToCgiValues,

         customEnvironmentSetToCgiValues,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.List

import qualified Data.Map as Map

import Wilde.Media.ElementSet

import           Wilde.Render.Cgi.ServerVariables
import qualified Wilde.Driver.Application.Cgi.VariableNames as VariableNames

import Wilde.Application.ApplicationInput


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Translates 'Input' to CGI values.
inputToCgiValues :: Input -> ServerVariables
inputToCgiValues input = customEnvironmentSetToCgiValues (customEnvironment input) ++
                         setToCgiValues (inputMedia input)

-- | Translates a 'ElementSet' that represents the CustomEnvironment
-- to CGI values.
customEnvironmentSetToCgiValues :: ElementSet -> ServerVariables
customEnvironmentSetToCgiValues = map addKeyPrefix . setToCgiValues
  where
    addKeyPrefix :: (String,Maybe String) -> (String,Maybe String)
    addKeyPrefix (k,v) = (customEnvironmentKeyPrefix ++ k,v)

setToCgiValues :: ElementSet -> ServerVariables
setToCgiValues = flattenValues . Map.toAscList
  where
    flattenValues :: [(String,[String])] -> ServerVariables
    flattenValues []     = []
    flattenValues (x:xs) = flattenValue x xs

    flattenMbValue :: (String,[String]) -> [(String,[String])] -> ServerVariables
    flattenMbValue (key,[])   xs = (key,Nothing) : flattenValues xs
    flattenMbValue v          xs = flattenValue v xs

    flattenValue :: (String,[String]) -> [(String,[String])] -> ServerVariables
    flattenValue (_  ,[])   xs = flattenValues xs
    flattenValue (key,v:vs) xs = (key,Just v) : flattenValue (key,vs) xs

-- | Translates HTTP server input to 'Input'.
-- TODO rename
inputFromCgiValues :: ServerVariables -> Input
inputFromCgiValues inputs = Input (revValues es1) (revValues es2)
  where
    Input es1 es2 = foldl ins (Input Map.empty Map.empty) inputs

    ins :: Input -> (String,Maybe String) -> Input
    ins (Input media env) (k,v) =
      case isCustEnvKey k of
        Nothing -> (Input (insElementSet media (k,v)) env)
        Just k' -> (Input media                      (insElementSet env (k',v)))

    insElementSet        :: ElementSet -> (String,Maybe String) -> ElementSet
    insElementSet m (k,mbV) = Map.insertWith (++) k vs m
      where
        vs = maybe [] (:[]) mbV

    revValues :: Map.Map a [b] -> Map.Map a [b]
    revValues = Map.map reverse

-- | Tells if a given key denotes a key in the CustomEnvironment.
--
-- If it is a key in the CustomEnvironment, then the result
-- is Just s, and s is the key in the environment.
isCustEnvKey :: String -> Maybe String
isCustEnvKey = stripPrefix customEnvironmentKeyPrefix

-- | Prefix of the key for all elements that belong to the
-- Custom Environment.
customEnvironmentKeyPrefix :: String
customEnvironmentKeyPrefix = VariableNames.customEnvironment ++ [keyPartsSeparator]

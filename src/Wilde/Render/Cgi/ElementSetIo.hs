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
import qualified Wilde.Render.Cgi.VariableNames as VariableNames

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

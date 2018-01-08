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

-- | Types for the interface between the servier and the application driver
-- for passing \"variables\".
module Wilde.Render.Cgi.ServerVariables
       (
         -- * Types
         ServerVariables,
         ServerVariableElement,
         
         -- * Translations for interoperability with alternative representations
         
         toMandatoryValue,
         toOptionalValue,
         toOptionalValues,
       )
       where


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Information delivered between the HTTP server and the
-- application driver.
type ServerVariables = [ServerVariableElement]

-- | (key,value) for a \"server variable\".
--
-- A multi-value variable is represented by several such \"variable elements\".
type ServerVariableElement = (String,Maybe String)

-- | Translates a 'ServerVariableElement' to an element with
-- a mandatory value.
--
-- The transformation: 'Nothing' is tranlated to the empty string.
toMandatoryValue :: ServerVariableElement -> (String,String)
toMandatoryValue (key,mbValue) = (key,maybe "" id mbValue)

-- | Translates a 'ServerVariableElement' to an element with
-- a mandatory value.
--
-- The transformation: 'Nothing' is tranlated to the empty string.
toOptionalValue :: (String,String) -> ServerVariableElement
toOptionalValue (key,value) = (key,Just value)

-- | List version of 'toOptionalValue'.
toOptionalValues :: [(String,String)] -> ServerVariables
toOptionalValues = map toOptionalValue

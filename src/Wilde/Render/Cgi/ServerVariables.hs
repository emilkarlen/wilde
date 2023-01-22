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

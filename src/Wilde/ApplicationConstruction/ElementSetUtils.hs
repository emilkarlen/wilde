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

-- | Utilities related to 'ElementSet's.
--
-- Especially, here are functionallity for looking up
-- values in a set, and parsing these values.
module Wilde.ApplicationConstruction.ElementSetUtils
       (
         -- * Lookuper:s
         
         lookupSingleton_maybeTrim,
         lookupSingleton_optional_maybeTrim,
         lookup_justAnythingIsTrue,

         -- * Parser:s

         lookupSingleton_maybeTrim_parser,

         justAnythingIsTrue_parser,
         
         -- ** For GenericStringRep
         
         gsr_lookuper,
         
         gsr_parser_fromGsr,
         gsr_parser,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Control.Monad

import Wilde.Media.ElementSet

import qualified Wilde.Media.GenericStringRep as GSR


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


lookupSingleton_maybeTrim :: Bool -- ^ If True, only whitespace is regarded as a missing value.
                          -> ElementKey
                          -> Lookuper ElementValue
lookupSingleton_maybeTrim trimedEmptyIsMissing =
  mkLookuper (lookupSingleton_maybeTrim_parser trimedEmptyIsMissing)

lookupSingleton_optional_maybeTrim :: Bool -- ^ If True, only whitespace is regarded as Nothing.
                                   -> ElementKey
                                   -> Lookuper (Maybe ElementValue)
lookupSingleton_optional_maybeTrim timEmptyIsNothing =
  mkLookuper (lookupSingleton_optional_maybeTrim_converter timEmptyIsNothing)

lookup_justAnythingIsTrue :: ElementKey
                   -> Lookuper Bool
lookup_justAnythingIsTrue = mkLookuper justAnythingIsTrue_parser

lookupSingleton_optional_maybeTrim_converter :: Bool -- ^ If True, only whitespace is regarded as Nothing.
                                             -> Parser (Maybe [ElementValue]) (Maybe ElementValue)
lookupSingleton_optional_maybeTrim_converter False = singleton_optional
lookupSingleton_optional_maybeTrim_converter True  = singleton_optional >=> nothingIsNothing2 trimEmptyIsNothing

lookupSingleton_maybeTrim_parser :: Bool
                                    -> Parser (Maybe [ElementValue]) ElementValue
lookupSingleton_maybeTrim_parser False = singleton_mandatory
lookupSingleton_maybeTrim_parser True  = singleton_mandatory >=> trimEmptyIsMissing

justAnythingIsTrue_parser :: Parser (Maybe a) Bool
justAnythingIsTrue_parser = maybe (return False) (const $ return True)


-------------------------------------------------------------------------------
-- - GenericStringRep -
-------------------------------------------------------------------------------


-- | Input is supposed to be a single value.
gsr_parser_fromGsr :: GSR.GenericStringRepInputer a 
                   -> Parser ElementValue a
gsr_parser_fromGsr gsrInputer = gsrInputer

-- | Input is supposed to be a single value.
gsr_parser :: GSR.GenericStringRepInputer a 
           -> Parser (Maybe [ElementValue]) a
gsr_parser gsrInputer = singleton_mandatory >=> gsrInputer

-- | Input is supposed to be a single value.
gsr_lookuper :: GSR.GenericStringRepInputer a
             -> ElementKey
             -> Lookuper a
gsr_lookuper gsrInputer = mkLookuper (gsr_parser gsrInputer)

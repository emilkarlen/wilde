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

{-# LANGUAGE FlexibleContexts #-}

-- | Input/Output of \"Generic String Representation\" for some types.
module Wilde.ApplicationConstruction.GenericStringRepIo
       (
         module Wilde.ObjectModel.GenericStringRep,
         
         AttributeGenericStringRepIo(..),

         atGsrIo_string,
         
         atGsrIo_showRead_nonEmpty,
         gsrIo_showRead_nonEmpty,
         
         atGsrIo_convertibleFromInteger_nonEmpty,
         gsrIo_convertibleFromInteger_nonEmpty,
         
         gsr_parser_read_nonEmpty,
         gsr_parser_convertibleFromInteger,
         
         atGsrIo_optionalForCreate,
         atGsrIo_optional_ValueMissing_is_Nothing,
         gsrIo_optional_ValueMissing_is_Nothing,
         
         gsrO_optional_from_mandatory_Nothing_isEmpty,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Utils.Utils

import Wilde.Media.ElementSet
import Wilde.Media.GenericStringRep

import Wilde.ObjectModel.ObjectModel
import Wilde.ObjectModel.GenericStringRep

import qualified Wilde.Media.ElementSet as ES


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------



  
-------------------------------------------------------------------------------
-- - AttributeGenericStringRepIo -
-------------------------------------------------------------------------------


-- | IO for an 'AttributeType' for the \"User Interaction
-- Generic String Representation\".
--
-- May need more sofisticated type for access to
-- IO, database.
data AttributeGenericStringRepIo typeForExisting typeForCreate =
  AttributeGenericStringRepIo
  {
    agsrioExistingIo :: GenericStringRepIo typeForExisting
  , agsrioCreateIo   :: GenericStringRepIo typeForCreate
  }

atGsrIo_string :: AttributeGenericStringRepIo String String
atGsrIo_string = AttributeGenericStringRepIo gsrIo gsrIo
  where
    gsrIo = GenericStringRepIo return id

-- | A 'AttributeGenericStringRepIo' for types that implement
-- 'Read' and 'Show', and who's 'show' always gives
-- a non-empty string.
--
-- NOTE: If the type is convertible from 'Integral', use
-- 'atGsrIo_convertibleFromInteger_nonEmpty' instead.
atGsrIo_showRead_nonEmpty :: (Show a,Read a)
                          => AttributeGenericStringRepIo a a
atGsrIo_showRead_nonEmpty = AttributeGenericStringRepIo gsrIo gsrIo
  where
    gsrIo  = gsrIo_showRead_nonEmpty

-- | A 'GenericStringRepIo' for a type that implement
-- 'Read' and 'Show', and who's 'show' always gives
-- a non-empty string.
--
-- NOTE: If the type is convertible from 'Integral', use
-- 'atGsrIo_convertibleFromInteger_nonEmpty' instead.
gsrIo_showRead_nonEmpty :: (Show a,Read a)
                        => GenericStringRepIo a
gsrIo_showRead_nonEmpty = GenericStringRepIo inputer outputer
  where
    outputer = show
    inputer  = gsr_parser_read_nonEmpty

-- | A 'AttributeGenericStringRepIo' for types that implement
-- 'Show' and 'Convertible' from 'Integral, AND who's 'show' always gives
-- a non-empty string.
atGsrIo_convertibleFromInteger_nonEmpty :: (Show a,Integral a,
                                            Convertible Integer a)
                                        => AttributeGenericStringRepIo a a
atGsrIo_convertibleFromInteger_nonEmpty =
  AttributeGenericStringRepIo gsrIo gsrIo
  where
    gsrIo  = gsrIo_convertibleFromInteger_nonEmpty

-- | A 'GenericStringRepIo' for a type that implements
-- 'Show' and 'Convertible' from 'Integral, AND who's 'show' always gives
-- a non-empty string.
gsrIo_convertibleFromInteger_nonEmpty :: (Show a,Integral a
                                         ,Convertible Integer a)
                                      => GenericStringRepIo a
gsrIo_convertibleFromInteger_nonEmpty =
  GenericStringRepIo inputer outputer
  where
    outputer = show
    inputer  = gsr_parser_convertibleFromInteger


-------------------------------------------------------------------------------
-- - utils -
-------------------------------------------------------------------------------


-- | A parser using 'Read' that assumes that the
-- representation in terms of 'GenericStringRep' is
-- always a non-empty string.
gsr_parser_read_nonEmpty :: Read a
                         => GenericStringRep
                         -> Either ES.ElementLookupErrorType a
gsr_parser_read_nonEmpty "" = Left ES.ValueMissing
gsr_parser_read_nonEmpty s =
  maybe
  (Left ES.InvalidSyntax)
  Right
  (readCompletelyAndUnambigously s)
  
gsr_parser_convertibleFromInteger :: (Show a,Integral a
                                     ,Convertible Integer a)
                                  => GenericStringRep
                                  -> Either ES.ElementLookupErrorType a
gsr_parser_convertibleFromInteger "" = Left ES.ValueMissing
gsr_parser_convertibleFromInteger s =
  maybe
  (Left ES.InvalidSyntax)
  convert
  (readCompletelyAndUnambigously s)
  where
    convert :: Convertible Integer a
            => Integer -> Either ES.ElementLookupErrorType a
    convert x =
      case safeConvert x of
      Left _  -> Left ES.InvalidValue
      Right y -> Right y

-- | Translates a 'AttributeGenericStringRepIo' to one who's
-- type-for-create is 'Maybe' type-for-existing.
--
-- The inputer for type-for-create uses the inputer for type-for-existing
-- but treats 'UiI.ValueMissing' as 'Nothing'.
atGsrIo_optionalForCreate :: AttributeGenericStringRepIo e c'
                                         -> AttributeGenericStringRepIo e (Maybe e)
atGsrIo_optionalForCreate (AttributeGenericStringRepIo existingIo _) =
  AttributeGenericStringRepIo
  existingIo
  (gsrIo_optional_ValueMissing_is_Nothing existingIo)

-- | Translates a 'AttributeGenericStringRepIo' to one who's
-- type-for-existing and type-for-create are both the type-for-existing of the
-- given object.
atGsrIo_optional_ValueMissing_is_Nothing :: AttributeGenericStringRepIo e c
                                                        -> AttributeGenericStringRepIo (Maybe e) (Maybe c)
atGsrIo_optional_ValueMissing_is_Nothing (AttributeGenericStringRepIo existingIo createIo) =
  AttributeGenericStringRepIo
  (gsrIo_optional_ValueMissing_is_Nothing existingIo)
  (gsrIo_optional_ValueMissing_is_Nothing createIo)

gsrIo_optional_ValueMissing_is_Nothing :: GenericStringRepIo a
                                          -> GenericStringRepIo (Maybe a)
gsrIo_optional_ValueMissing_is_Nothing (GenericStringRepIo inputer outputer) =
  GenericStringRepIo
  { gsrInputer  = parser_optional_ValueMissing_is_Nothing inputer
  , gsrOutputer = gsrO_optional_from_mandatory_Nothing_isEmpty outputer
  }


-------------------------------------------------------------------------------
-- - rendition -
-------------------------------------------------------------------------------


-- | Translates a parser for mandatory values to one that parses
-- an optional value.
--
-- For the resulting renderer to be one-to-one, it is required
-- that the given renderer-of-mandatory does not render any value
-- as the empty string.
--
-- The inversion of 'gsrO_optional_from_mandatory_Nothing_isEmpty' iff
-- a mandatory values is rendered as a non-empty string.
gsrO_optional_from_mandatory_Nothing_isEmpty :: (a -> String)
                                                 -> (Maybe a -> String)
gsrO_optional_from_mandatory_Nothing_isEmpty _ Nothing = ""
gsrO_optional_from_mandatory_Nothing_isEmpty renderer (Just x) = renderer x

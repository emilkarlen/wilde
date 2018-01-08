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

{-# LANGUAGE ExistentialQuantification #-}

-------------------------------------------------------------------------------
-- | Utilities for lookup of CLI flags/options.
--
-- /Selection of \"elements\" of a \"model\"/
--
-- Part of this module is functionality for selecting one or more \"elements\"
-- from a \"model\".
--
-- The model may for example be the Object Model of the application that is
-- developed using Wilde.
--
-- Many commands want to operate on one or more \"elements\" of this module.
-- Typical elements are Object Types and Object Type Setups.
--
-- The idea here is that the \"elements\" that the command should operate on
-- may be selected in two ways:
--
-- 1. Via a CLI flag/option that selects a single element.
--
-- 2. All elements of the model.
--    This case is used if the flag/option for selecting a single element is
--    absent.
--
-- /TODO/
--
-- Generalize to handle flags/options that may select more than one element
-- (e.g. options that may be used more than once).
-------------------------------------------------------------------------------
module Wilde.ApplicationTool.FlagLookup.Utils
       (

         -- * Selection of \"elements\" of a \"model\"

         -- ** Configuration

         ElementTypeAndFlagConfig(..),
         ElementTypeConfig(..),
         FlagConfig(..),

         -- ** Lookup

         lookupSelection,

         lookupSelection_single,
         lookupSelection_oneOrMore,

         -- * Lookup of CLI flags/options

         flagMandatory,
         flagOptional,

         -- * Error handling

         msgImplErrorLookup,
         msgFailArgMissing,

         msgFail,

         implError,
         implErrorForOption,

         -- * Combinators for flags/options

         mkMandatory,
         mkMandatoryIO,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.List

import qualified Wilde.Utils.NonEmptyList as NonEmpty

import Wilde.ApplicationTool.FlagsAndOptions as FlagsAndOptions

import Wilde.ApplicationTool.ErrorHandling


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Complete information about \"model\", \"elements\" and CLI flags
-- needed by the utilities that looks up \"elements\" from a \"model\".
-------------------------------------------------------------------------------
data ElementTypeAndFlagConfig m e =
     ElementTypeAndFlagConfig
     {
       elementTypeConfig              :: ElementTypeConfig m e
     , flagForSelectingASingleElement :: FlagConfig
     }

-------------------------------------------------------------------------------
-- | All information about a \"model\" and \"element\" needed for lookup of
-- \"elements\" from a \"model\".
-------------------------------------------------------------------------------
data ElementTypeConfig m e =
  ElementTypeConfig
  {
    elementTypeName                :: String
    -- ^ Describes the type of element we are
    -- looking for, for purpose of error messages, primarily.
  , getAllElementsOfModel          :: m -> [e]
  , getElementId                   :: e -> String
  }

-------------------------------------------------------------------------------
-- | All information about a CLI flag/option needed for lookup of
-- \"elements\" from a \"model\".
-------------------------------------------------------------------------------
data FlagConfig =
  FlagConfig
  {
    flagOptionName  :: String
                       -- ^ Name of the flag,
                       -- for purpose of error messages, primarily.
  , flagLookup      :: [Flag] -> Maybe String
  }

-------------------------------------------------------------------------------
-- | Finds a selection of elements.
--
-- An \"model\" is given, from which all elements can be extracted.
--
-- Each element is identified by a String, and a function that gives this
-- String is supplied as argument.
--
-- The selection is specified in one of two ways:
--
-- 1. By a flag that specifies the ID string of a single element.
--
-- 2. By an absent flag, in which case the selection is
--    all elements of the model.
--
-- Result:
--
-- [Left name]
-- if the flag is specified but does not denote an existing element.
-- The string is the value of the flag = the ID of the element.
--
-- [Just []]
-- The flag is not specified and the list of elements is empty.
--
-- [Just [x]]
-- Either the flag is not specified and the model contains
-- exactly one element;
-- or the flag is specified and one element matches.
--
-- [Just (x:xs)]
-- The flag is not specified and the model contains
-- more than one element.
-------------------------------------------------------------------------------
lookupSelection :: (m -> [t])
                -> (t -> String)
                -> ([Flag] -> Maybe String)
                -- ^ Lookup of the flag that selects a single element.
                -- Gives 'Nothing' if the flag is not present.
                -- Otherwise, the value is the ID of the selected element.
                -> [Flag]
                -> m
                -> Either String [t]
lookupSelection getAllElementsOfModel getElementId lookupFlag flags model =
    case lookupFlag flags of
      Nothing -> Right $ getAllElementsOfModel model
      Just elementId  -> maybe
                         (Left elementId)
                         (Right . (:[]))
                         (findElement elementId)
  where
    findElement elementId = find
                            (\element -> getElementId element == elementId)
                            (getAllElementsOfModel model)

-------------------------------------------------------------------------------
-- | Finds a single element.
--
-- Quits the application if not exactly one element is found.
--
-- The element can be found in two ways:
--
-- 1. Via a CLI flag/option
--
-- 2. By getting all elements of the model, if the model happens to contain
--    exactly one element (rare circumstance, though) .
-------------------------------------------------------------------------------
lookupSelection_single :: ElementTypeAndFlagConfig m a
                       -> m
                       -> [Flag]
                       -> IO a
lookupSelection_single (ElementTypeAndFlagConfig
                      {
                        elementTypeConfig              = theElementTypeConfig
                      , flagForSelectingASingleElement = theFlagConfig
                      })
  model
  flags
  =
  either
  noElementExistWithTheSpecifiedName
  zeroOrMoreElementsSpecified
  eitherInvalidFlagValueOrListOfElements
  where
    eitherInvalidFlagValueOrListOfElements =
      lookupSelection
      (getAllElementsOfModel theElementTypeConfig)
      (getElementId theElementTypeConfig)
      (flagLookup theFlagConfig)
      flags
      model

    noElementExistWithTheSpecifiedName :: String -> IO a
    noElementExistWithTheSpecifiedName flagValue = msgFail reason
      where
        reason = "Cannot find " ++
                 typeName ++ ": " ++
                 flagValue

    zeroOrMoreElementsSpecified :: [a] -> IO a

    zeroOrMoreElementsSpecified [e] = return e

    zeroOrMoreElementsSpecified [] = msgFail reason
      where
        reason = "The model does not contain any " ++
                 typeName ++ " objects"

    zeroOrMoreElementsSpecified (x:xs) = msgFail reason
      where
        reason = "Exactly one " ++ typeName ++
                 " must be selected using " ++
                 (flagOptionName theFlagConfig)

    typeName = elementTypeName theElementTypeConfig

-------------------------------------------------------------------------------
-- | Helper for selecting one or more elements.
--
-- Quits the application if none is specified/found.
--
-- The elements can be found in two ways:
--
-- 1. Via a given CLI option/flag
--
-- 2. By getting all elements of the model.
-------------------------------------------------------------------------------
lookupSelection_oneOrMore :: ElementTypeAndFlagConfig m a
                          -> m
                          -> [Flag]
                          -> IO (NonEmpty.List a)
lookupSelection_oneOrMore (ElementTypeAndFlagConfig
                      {
                        elementTypeConfig              = theElementTypeConfig
                      , flagForSelectingASingleElement = theFlagConfig
                      })
  model
  flags
  =
  either
  noElementExistWithTheSpecifiedName
  zeroOrMoreElementsSpecified
  eitherInvalidFlagValueOrListOfElements
  where
    eitherInvalidFlagValueOrListOfElements =
      lookupSelection
      (getAllElementsOfModel theElementTypeConfig)
      (getElementId theElementTypeConfig)
      (flagLookup theFlagConfig)
      flags
      model

    noElementExistWithTheSpecifiedName :: String -> IO (NonEmpty.List a)
    noElementExistWithTheSpecifiedName flagValue = msgFail reason
      where
        reason = "Cannot find " ++
                 typeName ++ ": " ++
                 flagValue

    zeroOrMoreElementsSpecified :: [a] -> IO (NonEmpty.List a)

    zeroOrMoreElementsSpecified [] = msgFail reason
      where
        reason = "The model does not contain any " ++
                 typeName ++ " elements"

    zeroOrMoreElementsSpecified (x:xs) = return $ NonEmpty.mk x xs

    typeName = elementTypeName theElementTypeConfig

flagMandatory :: (Flag -> Bool) -> [Flag] -> String -> IO Flag
flagMandatory predicate flags flagName =
  maybe (msgFailArgMissing flagName) return $ flagOptional predicate flags

flagOptional :: (Flag -> Bool) -> [Flag] -> Maybe Flag
flagOptional = find

msgImplErrorLookup flag = msgFail $ "implementation error when looking for " ++ flag

msgFailArgMissing argName = msgFail $ argName ++ " is mandatory"

implErrorForOption :: Show opt => opt -> a
implErrorForOption = error . ("ImplementationError in flag lookup of: "++) . show

mkMandatory :: String -> Maybe a -> IO a
mkMandatory flagName mbVal = maybe (msgFailArgMissing flagName) return mbVal

mkMandatoryIO :: String -> IO (Maybe a) -> IO a
mkMandatoryIO flagName getMbVal =
  do
    mbVal <- getMbVal
    maybe (msgFailArgMissing flagName) return mbVal

-------------------------------------------------------------------------------
-- | Lookup functionallity on a set of 'Element's.
--
-- This functionallity is designed to match that of the CGI variables passed to
-- an application.
--
-- There is also functionality for \"integrating\" one or more such
-- 'ElementSet's into a monad that
-- has sets in it's state, and is able to handle the type of lookup errors defined here.
--
-- The lookup functions are designed to be easy to integrate into a monad.
--
-- This module is designed so that it should be easy for a monad to have multiple
-- such sets.
-------------------------------------------------------------------------------
module Wilde.Media.ElementSet
       (
         module Wilde.Media.Element,

         -- * ElementSet type and monad with ElementSet.

         ElementSet,

         empty,
         fromElements,

         -- * Errors and results

         ElementInputResult,
         ElementLookupError,
         ElementLookupErrorType(..),

         -- * Lookup functions

         LookupResult,
         Lookuper,
         throw,

         lookupRaw,
         lookupRaw',
         lookupRaw_stringKey,
         deleteRaw_stringKey,

         lookupSingleton,
         lookupSingleton_optional,

         -- * Integration into a monad

         ElementSetMonadIntegration(..),
         integrateLookup,
         integrateSetFunction,

         -- * Parser

         Parser,
         mkLookuper,

         parser_optional_ValueMissing_is_Nothing,
         nothingIs,
         nothingIsNothing,
         nothingIsNothing2,
         nothingIsValueMissing,
         trimEmptyIsNothing,
         trimEmptyIsMissing,
         singletonList,
         nonEmptyList,
         singleton_optional,
         singleton_mandatory,
         parse,
         parseReadable,
         forEach,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Char

import qualified Data.Map as Map

import Control.Monad

import Wilde.Media.Element


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Representation of a set.
--
-- A map : NameOfVariable -> Values
type ElementSet = Map.Map String [String]

-- | The empty 'ElementSet'.
empty :: ElementSet
empty = Map.empty

fromElements :: [Element] -> ElementSet
fromElements = Map.fromList .
               map (\(ek,ev) -> (elementKeyRender ek,[ev]))


-------------------------------------------------------------------------------
-- - Error types -
-------------------------------------------------------------------------------


-- | The result of inputing an 'Attribute'-related value from the UI input media.
type ElementInputResult a = Either ElementLookupError a

-- | The type of error that can occurr when \"looking up\" a value.
--
-- \"Looking up\" here means looking up AND possibly \"parsing\" a value.
--
-- TODO Split ValueMissing and (InvalidSyntax...) into different datatypes,
-- that (InvalidSyntax...) can be used as pure type for parsers.
-- Eg: Parser a b = a -> Either (InvalidSyntax...) b
data ElementLookupErrorType = ValueMissing  -- ^ The set does not contain a value for a given key.
                            | InvalidSyntax -- ^ Value is found in the set but has invalid syntax.
                            | InvalidValue  -- ^ Value is found in the set and has correct syntax, but the actual value is invalid.
                            deriving (Eq,Show)

-- | A \"lookup\" error - an error type togheter with information that may
-- help to produce a sensible error message.
type ElementLookupError = (ElementKey,ElementLookupErrorType,Maybe String)

-- The result of a lookup.
type LookupResult a = Either ElementLookupError a

-- | A lookup function on an 'ElementSet'.
type Lookuper a = ElementSet -> LookupResult a


-------------------------------------------------------------------------------
-- - Parser, and construction of Lookuper:s from such -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Parses the value in the set for a single key, to something that
-- may be regarded as the result of the lookup on the key.
--
-- The purpose of this type is to be able to used to construct complex lookup
-- function from simple ones.
--
-- Compose parses using '>=>' from "Control.Monad".
--
-- TODO: Maybe make it possible to include an (optional) error message in this
-- type.
-------------------------------------------------------------------------------
type Parser a b = a -> Either ElementLookupErrorType b

-------------------------------------------------------------------------------
-- | Translates a parser for mandatory values to one that parses
-- an optional value.
-------------------------------------------------------------------------------
parser_optional_ValueMissing_is_Nothing :: Parser a b
                                        -> Parser a (Maybe b)
parser_optional_ValueMissing_is_Nothing parser a =
  case parser a of
    Right x            -> Right (Just x)
    Left ValueMissing  -> pure Nothing
    Left err           -> Left err

-------------------------------------------------------------------------------
-- | Nothing is a valid value, and no further conversion of it is needed.
--
-- But if the input is (Just x), then furter conversion is neded.
-------------------------------------------------------------------------------
nothingIsNothing :: Parser a b
                 -> Parser (Maybe a) (Maybe b)
nothingIsNothing parser = maybe (pure Nothing) (fmap Just . parser)

-------------------------------------------------------------------------------
-- | A variant of 'nothingIsNothing'
--
-- (Do not know which one is needed yet.)
-------------------------------------------------------------------------------
nothingIsNothing2 :: Parser a         (Maybe b)
                  -> Parser (Maybe a) (Maybe b)
nothingIsNothing2 parser = maybe (pure Nothing) parser

-------------------------------------------------------------------------------
-- | Nothing is regarded as 'ValueMissing'.
-------------------------------------------------------------------------------
nothingIsValueMissing :: Parser (Maybe a) a
nothingIsValueMissing = maybe (Left ValueMissing) Right

-------------------------------------------------------------------------------
-- | A parser with a constant value for 'Nothing', and
-- another parser for 'Just'.
-------------------------------------------------------------------------------
nothingIs :: b
          -> Parser a         b
          -> Parser (Maybe a) b
nothingIs x p = maybe (pure x) p

trimEmptyIsNothing :: Parser String (Maybe String)
trimEmptyIsNothing s =
  case dropWhile isSpace s of
    [] -> pure Nothing
    s  -> pure $ Just s

trimEmptyIsMissing :: Parser String String
trimEmptyIsMissing s =
  case dropWhile isSpace s of
    [] -> Left ValueMissing
    s  -> pure s

-------------------------------------------------------------------------------
-- | The input must be a singleton list.
--
-- All other lists are regarded as 'InvalidSyntax'.
-------------------------------------------------------------------------------
singletonList :: Parser [ElementValue] ElementValue
singletonList [x] = pure x
singletonList xs  = Left InvalidSyntax

-------------------------------------------------------------------------------
-- | The input must be a non-empty list.
--
-- The empty list is regarded as 'InvalidValue'.
-------------------------------------------------------------------------------
nonEmptyList :: Parser [ElementValue] [ElementValue]
nonEmptyList [] = Left InvalidValue
nonEmptyList xs = pure xs

singleton_optional :: Parser (Maybe [ElementValue]) (Maybe ElementValue)
singleton_optional = nothingIsNothing singletonList

singleton_mandatory :: Parser (Maybe [ElementValue]) ElementValue
singleton_mandatory = nothingIsValueMissing >=> singletonList

-------------------------------------------------------------------------------
-- | Parses a value according to a parser
-------------------------------------------------------------------------------
parse :: (ElementValue -> Maybe a)
      -> Parser ElementValue a
parse parser = maybe (Left InvalidSyntax) Right . parser

-------------------------------------------------------------------------------
-- | Parses a value using the definition of 'Read'.
-------------------------------------------------------------------------------
parseReadable :: Read a => Parser ElementValue a
parseReadable = parse readParser
  where
    readParser x = case readsPrec 0 x of
      [(value,"")] -> Just value
      _            -> Nothing

-------------------------------------------------------------------------------
-- | Applies a parser of a single element to all elements of a list.
-------------------------------------------------------------------------------
forEach :: Parser a b
        -> Parser [a] [b]
forEach = flip forM


-------------------------------------------------------------------------------
-- - lookup functions -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Construct a 'Lookuper' from a 'Parser'.
--
-- (The 'Lookuper' can be used together with 'integrateLookup'.)
-------------------------------------------------------------------------------
mkLookuper :: Parser (Maybe [ElementValue]) a
           -> ElementKey
           -> Lookuper a
mkLookuper parser ek set =
  let
    mbValues = lookupRaw set ek
  in
    case parser mbValues of
      Left errType -> throw (ek,errType,maybe Nothing (Just . show) mbValues)
      Right a      -> pure a

throw :: ElementLookupError -> LookupResult a
throw = Left

lookupRaw :: ElementSet -> ElementKey -> Maybe [ElementValue]
lookupRaw set ek = Map.lookup (elementKeyRender ek) set

lookupRaw' :: ElementKey -> ElementSet -> Maybe [ElementValue]
lookupRaw' ek set = Map.lookup (elementKeyRender ek) set

lookupRaw_stringKey :: String -> ElementSet -> Maybe [ElementValue]
lookupRaw_stringKey key set = Map.lookup key set

deleteRaw_stringKey :: String -> ElementSet -> ElementSet
deleteRaw_stringKey = Map.delete

-- | Looks up a mandatory singleton value.
lookupSingleton :: ElementKey -> Lookuper ElementValue
lookupSingleton = mkLookuper singleton_mandatory

-- | Looks up an optional singleton value.
lookupSingleton_optional :: ElementKey
                         -> Lookuper (Maybe ElementValue)
lookupSingleton_optional = mkLookuper singleton_optional


-------------------------------------------------------------------------------
-- - monad integration -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Integration of an 'Element Set' into some monad.
--
-- \"some monad\" will probably usually be a monad with (some sort of)
-- state and error.
--
-- Putting the integration in a separate object makes it possible for a monad
-- to have multiple 'ElementSet's.
-- data ElementSetMonadIntegration =
-------------------------------------------------------------------------------
data ElementSetMonadIntegration m a =
  ElementSetMonadIntegration
  {
    getElementSet :: m ElementSet
  , throwError    :: ElementLookupError -> m a
  }

-------------------------------------------------------------------------------
-- | \"Integrates\" lookup functionality on a specific 'ElementSet' into a monad.
--
-- It is easy to implement a method
--
-- > lookupInTheSetX :: Lookuper a -> m a
--
-- for a monad m that has an \"integrated\" 'ElementSet' called \"X\".
-------------------------------------------------------------------------------
integrateLookup :: Monad m
                => ElementSetMonadIntegration m a
                -> Lookuper a
                -> m a
integrateLookup (ElementSetMonadIntegration getSet throwError) lookup =
  do
    set <- getSet
    let lookupResult = lookup set
    either throwError pure lookupResult

-------------------------------------------------------------------------------
-- | \"Integrates\" functionality on a specific 'ElementSet' into a monad.
--
-- This function is a variant of 'integrateLookup'.
-------------------------------------------------------------------------------
integrateSetFunction :: Monad m
                     => ElementSetMonadIntegration m a
                     -> (ElementSet -> a)
                     -> m a
integrateSetFunction (ElementSetMonadIntegration getSet _) f =
  do
    set <- getSet
    pure $ f set

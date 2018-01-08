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

-- | Definition of \"element\" - simple (Key,Value) pair of strings.
--
-- Such (Key,Value) pairs is the media used for, e.g., User Interaction.
module Wilde.Media.Element
       (
         -- * Element

         Element,
         element,
         ElementKey,
         ElementKeyPrefix,
         ElementKeyLeaf,
         ElementValue,
         elementValueForFlags,
         
         -- * Element Key

         elementKey,
         globalElementKey,
         elementKeyPrefixFromString,
         elementKeyPrefixAdd,
         elementKeyAsPrefix,
         elementKeyPrefixAsKey,

         elementKeyRender,
         elementKeyParse,

         keyPartsSeparator,
         
         -- * List values

         -- | Method for managing list values.

         elementValueEncodeList,
         elementValueDecodeList,

         elementValueEncodeElementKey,
         elementValueDecodeElementKey,

         elementValueEncodeElementKeyPrefix,
         elementValueDecodeElementKeyPrefix,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Utils.ListUtils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Element -
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | An \"element\" is a (key,value) object.
--
-- All information processed via \"User Interaction\" is
-- represented by elements.
-------------------------------------------------------------------------------
type Element = (ElementKey,ElementValue)

-------------------------------------------------------------------------------
-- | The value part of an 'Element'.
-------------------------------------------------------------------------------
type ElementValue = String

elementValueForFlags :: ElementValue
elementValueForFlags = "_"

-------------------------------------------------------------------------------
-- | An \"element key\" identifies an element.
--
-- They have a nested struture, to make it easier to
-- avoid clashes.
--
-- Nesting is acomplished by having a list of prefixes, each representing
-- a level of nesting.
--
-- A key that has an empty list of prefixes is \"global\".
--
-- The "leaf" part of the key - the String - must not be empty.
-------------------------------------------------------------------------------
type ElementKey = (ElementKeyPrefix,ElementKeyLeaf)

-- | Constructs an 'Element'.
element :: ElementKey -> ElementValue -> Element
element key value = (key,value)

elementKey :: ElementKeyPrefix -> ElementKeyLeaf -> ElementKey
elementKey prefix leaf = (prefix,leaf)

-------------------------------------------------------------------------------
-- | The \"leaf\" part of an 'ElementKey'.
-------------------------------------------------------------------------------
type ElementKeyLeaf = String

-------------------------------------------------------------------------------
-- | Prefix of an 'ElementKey'.
-------------------------------------------------------------------------------
type ElementKeyPrefix = [String]

-------------------------------------------------------------------------------
-- | Makes an 'ElementKeyPrefix' that consists of a single part.
-------------------------------------------------------------------------------
elementKeyPrefixFromString :: String -> ElementKeyPrefix
elementKeyPrefixFromString = (:[])

-------------------------------------------------------------------------------
-- | Adds a part of a 'ElementKeyPrefix'.
-------------------------------------------------------------------------------
elementKeyPrefixAdd :: ElementKeyPrefix -> String -> ElementKeyPrefix
elementKeyPrefixAdd ekp s = ekp ++ [s]

-------------------------------------------------------------------------------
-- | An 'ElementKey' with an empty prefix.
-------------------------------------------------------------------------------
globalElementKey :: String -> ElementKey
globalElementKey s = ([],s)

-------------------------------------------------------------------------------
-- | Transforms an 'ElementKey' to a prefix.
-------------------------------------------------------------------------------
elementKeyAsPrefix :: ElementKey -> ElementKeyPrefix
elementKeyAsPrefix (pfx,leaf) = pfx ++ [leaf]

-------------------------------------------------------------------------------
-- | Transforms an 'ElementKeyPrefix' to an 'ElementKey'.
--
-- If the list of prefixes is empty, the result is an empty key.
-------------------------------------------------------------------------------
elementKeyPrefixAsKey :: ElementKeyPrefix -> ElementKey
elementKeyPrefixAsKey []     = ([],"")
elementKeyPrefixAsKey [x]    = ([],x)
elementKeyPrefixAsKey [x,y]  = ([x],y)
elementKeyPrefixAsKey (x:xs) =
  case reverse xs of
    []     -> ([],x)
    (y:ys) -> (reverse ys,y)

-------------------------------------------------------------------------------
-- | Renders an 'ELementKey' as a string.
--
-- (There is currently only one way to render an 'ElementKey'.)
--
-- The value can be parsed by 'elementKeyParse'.
-------------------------------------------------------------------------------
elementKeyRender :: ElementKey -> String
elementKeyRender (pfxs,leaf) = concat $ (map (++".") pfxs) ++ [leaf]

-------------------------------------------------------------------------------
-- | Parses a string representation of an 'ElementKey'.
--
-- The string representation should have been produced by 'elementKeyRender'.
--
-- TODO: Maybe report error in a way that can be detected.
-- TODO: Maybe check all components of the constructed value for errors.
--
-- Checks may be motivated since this method will read info that comes
-- from the web, i.e. can have any form.
-------------------------------------------------------------------------------
elementKeyParse :: String -> ElementKey
elementKeyParse s =
  case splitOn keyPartsSeparator s of
    [] -> error $ "Wilde:elmentKeyParse: unparsable: '" ++ s ++ "'"
    (x:xs) -> build [] (x:xs)
        where
          build :: [String] -> [String] -> ElementKey
          build prefix [leaf]     = (reverse prefix,leaf)
          build prefix (x1:x2:xs) = build (x1:prefix) (x2:xs)


-------------------------------------------------------------------------------
-- - Encoding and decoding of some special kinds of values -
-------------------------------------------------------------------------------


-- | Encodes a list as a 'ElementKeyValue'.
elementValueEncodeList :: [String] -> ElementValue
elementValueEncodeList = renderCommaList

-- | Decodes a list value constructed by 'elementValueEncodeList'.
elementValueDecodeList :: ElementValue -> [String]
elementValueDecodeList = parseCommaList

elementValueEncodeElementKey :: ElementKey -> ElementValue
elementValueEncodeElementKey = elementKeyRender

elementValueDecodeElementKey :: ElementValue -> ElementKey
elementValueDecodeElementKey = elementKeyParse

elementValueEncodeElementKeyPrefix :: ElementKeyPrefix -> ElementValue
elementValueEncodeElementKeyPrefix = renderSingleCharSepaList keyPartsSeparator

elementValueDecodeElementKeyPrefix :: ElementValue -> ElementKeyPrefix
elementValueDecodeElementKeyPrefix = parseSingleCharSepaList keyPartsSeparator

-- | The character that separates the parts of an 'ElementKey' when it is rendered
-- as a String.
keyPartsSeparator :: Char
keyPartsSeparator = '.'

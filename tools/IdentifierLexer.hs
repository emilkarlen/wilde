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

-- | A exremely simple lexer that knows about white-space, Haskell comments, "identifiers".
module IdentifierLexer
       (
         Identifier,
         lexemes,
       )
       where

import qualified Data.Char as Char

type Identifier = String

lexemes :: String -> [Identifier]
lexemes = readIdentifiers . skipComments
  where
    readIdentifiers :: String -> [Identifier]
    readIdentifiers = maybe [] (\(lexeme,rest) -> lexeme : readIdentifiers rest) . getIdentifier
    
getIdentifier :: String -> Maybe (Identifier,String)
getIdentifier = readIdentifier . dropWhile Char.isSpace
  where
    readIdentifier :: String -> Maybe (Identifier,String)
    readIdentifier [] = Nothing
    readIdentifier xs = Just $ span (not . Char.isSpace) xs
  
skipComments :: String -> String
skipComments []               = []
skipComments ('-' : '-' : xs) = skipComments $ skipLine xs
skipComments ('{' : '-' : xs) = skipComments $ skipNonLineComment xs
skipComments (x : xs)         = x : skipComments xs

skipNonLineComment :: String -> String
skipNonLineComment []               = []
skipNonLineComment ('-' : '}' : xs) = xs
skipNonLineComment ('{' : '-' : xs) = skipNonLineComment $ skipNonLineComment xs
skipNonLineComment (x : xs)         = skipNonLineComment xs

skipLine = dropWhile (=='\n') . dropWhile (/='\n')

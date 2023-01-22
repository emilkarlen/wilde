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

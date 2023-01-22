-------------------------------------------------------------------------------
-- | Methods for evaluating mathematical numerical expressions.
--
-- The methods uses Haskell's syntax of numbers.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.UserInteraction.Input.EvaluateExpression
       (
         evalInteger,
         evalDouble,

         ParseError,
         errorString,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Char

import Data.Functor.Identity

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Error


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


errorString :: ParseError -> String
errorString = unlines . map messageString . errorMessages


-------------------------------------------------------------------------------
-- - Double -
-------------------------------------------------------------------------------


-- | Parses and evaluates a 'String' as a 'Double'.
--
-- Accepts space at the beginning and end of the string.
-- Does not accept other than space after the parsable part.
evalDouble :: String -> Either ParseError Double
evalDouble = eval exprDouble

exprDouble :: ParsecT String u Identity Double
exprDouble = buildExpressionParser tableDouble termDouble
          <?> "expression"

termDouble :: ParsecT String u Identity Double
termDouble =  parens exprDouble
          <|> fmap (either fromIntegral id) naturalOrFloat
          <?> "constant or expression in parenthesis"

tableDouble :: [[Operator String u Identity Double]]
tableDouble = [ [prefix "-" negate, prefix "+" id ]
              , [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft ]
              , [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft ]
              ]


-------------------------------------------------------------------------------
-- - Integer -
-------------------------------------------------------------------------------


-- | Parses and evaluates a 'String' as an 'Integer'.
--
-- Accepts space at the beginning and end of the string.
-- Does not accept other than space after the parsable part.
evalInteger :: String -> Either ParseError Integer
evalInteger = eval exprInteger

exprInteger :: ParsecT String u Identity Integer
exprInteger = buildExpressionParser tableInteger termInteger
           <?> "expression"

termInteger :: ParsecT String u Identity Integer
termInteger = parens exprInteger
           <|> natural
           <?> "constant or expression in parenthesis"

tableInteger :: [[Operator String u Identity Integer]]
tableInteger = [ [prefix "-" negate, prefix "+" id ]
               , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
               , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
               ]


-------------------------------------------------------------------------------
-- - Helper functions -
-------------------------------------------------------------------------------


-- Makes an evaluator from a parser.
--
-- Adds acceptance of space at the beginning of the input.
-- Removes acceptans of other than space at the end of the parsable input.
eval :: ParsecT String () Identity a -- ^ Parser of an expression,
                                     -- may stop (successfully) at end of parsable input.
        -> String                    -- ^ Input
        -> Either ParseError a
eval exprParser = parse parser theSourceName . dropWhile isSpace
  where
    theSourceName = "" -- Name of source file, only used for error messages.
    parser = do
      x <- exprParser
      eof
      return x

binary  name fun assoc = Infix   (reservedOp name >> return fun) assoc
prefix  name fun       = Prefix  (reservedOp name >> return fun)
postfix name fun       = Postfix (reservedOp name >> return fun)

-- Use a lexer that uses the Haskell grammar.
lexer          = P.makeTokenParser haskellDef
reservedOp     = P.reservedOp     lexer
parens         = P.parens         lexer
natural        = P.natural        lexer
naturalOrFloat = P.naturalOrFloat lexer

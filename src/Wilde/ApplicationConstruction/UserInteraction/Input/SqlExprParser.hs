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

-------------------------------------------------------------------------------
-- | Parser of SQL expressions.
--
-- This implementation is a slight modification of http://hpaste.org/76009.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.UserInteraction.Input.SqlExprParser
       (
         Expression(..),
         
         parseWithListedColumns,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Char (toLower)

import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr
import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>), Column)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data Expression tableColumn
  = IntLiteral Integer
  | FloatLiteral Double
  | StringLiteral String
  | CharLiteral Char
  | BoolLiteral Bool
  | Null
  | Pow   (Expression tableColumn) (Expression tableColumn)   -- '^'
  | Mul   (Expression tableColumn) (Expression tableColumn)   -- '*'
  | Div   (Expression tableColumn) (Expression tableColumn)   -- /
  | Mod   (Expression tableColumn) (Expression tableColumn)   -- %
  | Plus  (Expression tableColumn) (Expression tableColumn)   -- +
  | Minus (Expression tableColumn) (Expression tableColumn)   -- -
  | Lt    (Expression tableColumn) (Expression tableColumn)    -- <
  | Gt    (Expression tableColumn) (Expression tableColumn)    -- >
  | Lte   (Expression tableColumn) (Expression tableColumn)   -- <=
  | Gte   (Expression tableColumn) (Expression tableColumn)   -- >=
  | Eq    (Expression tableColumn) (Expression tableColumn)    -- =
  | Is    (Expression tableColumn) (Expression tableColumn)    -- IS
  | Like  (Expression tableColumn) (Expression tableColumn)  -- LIKE
  | And   (Expression tableColumn) (Expression tableColumn)   -- AND
  | Or    (Expression tableColumn) (Expression tableColumn)    -- OR
  | Pos   (Expression tableColumn)              -- Unary positive
  | Neg   (Expression tableColumn)              -- Unary complement
  | Not   (Expression tableColumn)              -- Logical complement
  | In    (Expression tableColumn) (Expression tableColumn)
  | ExprsInParens [Expression tableColumn] -- Either a set or a an expr surrounded by parens
  | ColumnVar tableColumn       -- column reference
  | FunCall String [Expression tableColumn]
  deriving (Show,Eq)

sqlStyle :: LanguageDef st
sqlStyle = emptyDef{ commentStart = "/*"
                   , commentEnd = "*/"
                   , commentLine = "--"
                   , nestedComments = True
                   , identStart = letter
                   , identLetter = alphaNum <|> char '_'
                   , reservedOpNames = [ ".", "+", "-", "^", "*"
                                       , "/", "%", "<", ">", "="]
                   , reservedNames = [ "IN", "IS" , "LIKE", "NOT", "AND", "OR" -- Operators
                                     , "TRUE", "FALSE", "NULL"         -- Literals
                                     ]
                   , caseSensitive = False
}

TokenParser{ identifier     = p_identifier
           , reserved       = p_reserved
           , reservedOp     = p_reservedOp
           , symbol         = p_symbol
           , lexeme         = p_lexeme
           , parens         = p_parens
           , naturalOrFloat = p_numeric
           , stringLiteral  = p_stringLiteral
           , charLiteral    = p_charLiteral
           , dot            = p_dot
           , comma          = p_comma
           , whiteSpace     = p_whiteSpace
           , semi           = p_semi
} = makeTokenParser sqlStyle

-- p_column :: Parser tableColumn
-- p_column = p_identifier
--             <?> "column"

{-
   | Operator  | Associativity | Description                        |
   | _________ | _____________ | __________________________________ |
   | + -       | right         | unary plus, unary minus            |
   | ^         | left          | exponentiation                     |
   | * / %     | left          | multiplication, division, modulo   |
   | + -       | left          | addition, subtraction              |
   | IS        | left          | IS TRUE, IS FALSE, IS NULL, etc.   |
   | < > <= >= | left          | less than, greater than            |
   | =         | right         | equality                           |
   | NOT       | right         | logical negation                   |
   | AND       | left          | logical conjunction                |
   | OR        | left          | logical disjunction                |
 -}

opTable = [ [ Prefix (Neg   <$ p_reservedOp    "-")
            , Prefix (Pos   <$ p_reservedOp    "+") ]
          , [ Infix  (Pow   <$ p_reservedOp    "^") AssocLeft ]
          , [ Infix  (Mul   <$ p_reservedOp    "*") AssocLeft
            , Infix  (Div   <$ p_reservedOp    "/") AssocLeft
            , Infix  (Mod   <$ p_reservedOp    "%") AssocLeft ]
          , [ Infix  (Plus  <$ p_reservedOp    "+") AssocLeft
            , Infix  (Minus <$ p_reservedOp    "-") AssocLeft ]
          , [ Infix  (Is    <$ p_reserved     "IS") AssocLeft ]
          , [ Infix  (Like  <$ p_reserved   "LIKE") AssocLeft ]
          , [ Infix  (In    <$ p_reserved     "IN") AssocLeft ]
          , [ Infix  (Lt    <$ p_reservedOp    "<") AssocLeft
            , Infix  (Gt    <$ p_reservedOp    ">") AssocLeft
            , Infix  (Lte   <$ p_reservedOp   "<=") AssocLeft
            , Infix  (Gte   <$ p_reservedOp   ">=") AssocLeft ]
          , [ Infix  (Eq    <$ p_reservedOp    "=") AssocRight ]
          , [ Prefix (Not   <$ p_reserved    "NOT") ]
          , [ Infix  (And   <$ p_reserved    "AND") AssocLeft ]
          , [ Infix  (Or    <$ p_reserved     "OR") AssocLeft ]
        ]

p_expression :: Parser tableColumn -> Parser (Expression tableColumn)
p_expression p_column = buildExpressionParser opTable p_expression' <?> "expression"
    where p_expression' = ExprsInParens     <$> p_set (p_expression p_column)
                      <|> BoolLiteral True  <$  p_reserved "TRUE"
                      <|> BoolLiteral False <$  p_reserved "FALSE"
                      <|> Null              <$  p_reserved "NULL"
                      <|> extractNum        <$> p_numeric
                      <|> StringLiteral     <$> p_stringLiteral
                      <|> CharLiteral       <$> p_charLiteral
                      <|> ColumnVar         <$> p_column
                      <|> p_funCall (p_expression p_column)
          extractNum (Left x) = IntLiteral x
          extractNum (Right x) = FloatLiteral x

-------------------------------------------------------------------------------
-- | A parser for expressions (for a single table), given a parser of columns
-- of the table.
-------------------------------------------------------------------------------
parseString :: Parser tableColumn
               -> String 
               -> Either ParseError (Expression tableColumn)
parseString p_column = parse (p_whiteSpace *> ((p_expression p_column) <* eof))
              "Invalid SQL"

-------------------------------------------------------------------------------
-- | A parser for expressions (for a single table), given all columns in the
-- table.
-------------------------------------------------------------------------------
parseWithListedColumns :: [(String,tableColumn)] -- ^ (column name in lower case,column)
                          -> String 
                          -> Either ParseError (Expression tableColumn)
parseWithListedColumns columns = parseString (columnParserForListedColumns columns)

p_funCall :: Parser (Expression tableColumn) -> Parser (Expression tableColumn)
p_funCall p_expression = try p
  where
    p = do
        ident <- p_identifier
        args  <- p_parens p_argList
        return $ FunCall ident args

    p_argList = p_expression `sepBy` p_comma

p_set :: Parser (Expression tableColumn) -> Parser [Expression tableColumn]
p_set p_expression = try p
  where
    p       = p_parens p_elems
    p_elems = p_expression `sepBy` p_comma

columnParserForListedColumns :: [(String,tableColumn)] -- ^ (column name in lower case,column)
                                -> Parser tableColumn
columnParserForListedColumns columns = try p
  where
    p = do
      ident <- p_identifier
      let identLower = map toLower ident
      let failMsg = "Not a column: " ++ ident
      maybe
        (fail failMsg)
        return $
        lookup identLower columns

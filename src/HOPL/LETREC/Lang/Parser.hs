{-
 -  HOPL/LETREC/Parser.hs
 -
 -  Reference implementation of the toy language LETREC from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the grammatical specification for LETREC.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LETREC.Lang.Parser
  ( parseToplevel,
    ParseError,
  )
where

import HOPL.LETREC.Lang.Lexer
import HOPL.LETREC.Lang.Syntax (Exp (..), Pgm (..))
import Text.Parsec (ParseError, choice, eof, many1, parse, sepBy, try)
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)

parseToplevel :: String -> Either ParseError Pgm
parseToplevel = parse (contents toplevel) "<stdin>"

toplevel :: Parser Pgm
toplevel = program

parseExp :: String -> Either ParseError Exp
parseExp = parse (contents expression) "<stdin>"

contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

{- Grammar for the PROC language -}

program :: Parser Pgm
program = Pgm <$> expression

expression :: Parser Exp
expression =
  (choice . map try)
    [ -- Variable declarations
      UnpackExp
        <$> (reserved "unpack" >> many1 identifier)
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      LetExp
        <$> (reserved "let" >> identifier)
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      LetrecExp
        <$> (reserved "letrec" >> identifier)
        <*> parens identifier
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      -- Control expressions
      IfExp
        <$> (reserved "if" >> expression)
        <*> (reserved "then" >> expression)
        <*> (reserved "else" >> expression),
      -- Function definition
      ProcExp
        <$> (reserved "proc" >> parens identifier)
        <*> expression,
      -- Function call
      CallExp
        <$> (symbol "(" >> expression)
        <*> (expression <* symbol ")"),
      -- List constructors
      EmptyExp
        <$ reserved "emptylist",
      ListConsExp
        <$> (reserved "cons" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      ListExp
        <$> (reserved "list" >> parens (sepBy expression (symbol ","))),
      -- List observers
      IsNullExp
        <$> (reserved "null?" >> parens expression),
      CarExp
        <$> (reserved "car" >> parens expression),
      CdrExp
        <$> (reserved "cdr" >> parens expression),
      -- Logical operators
      AndExp
        <$> (reservedOp "&&" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      OrExp
        <$> (reservedOp "||" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      NotExp
        <$> (reserved "not" >> parens expression),
      -- Relational operators
      IsEqualExp
        <$> (reservedOp "==" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      IsNotEqualExp
        <$> (reservedOp "==" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      IsLessExp
        <$> (reservedOp "<" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      IsGreaterExp
        <$> (reservedOp ">" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      IsLessEqExp
        <$> (reservedOp "<=" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      IsGreaterEqExp
        <$> (reservedOp ">=" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      -- Arithmetic operators
      DiffExp
        <$> (reservedOp "-" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      SumExp
        <$> (reservedOp "+" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      ProdExp
        <$> (reservedOp "*" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      DivExp
        <$> (reservedOp "/" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      ModExp
        <$> (reservedOp "%" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      MinusExp
        <$> (reserved "minus" >> parens expression),
      -- Arithmetic/numeric predicates
      IsZeroExp
        <$> (reserved "zero?" >> parens expression),
      IsNegExp
        <$> (reserved "neg?" >> parens expression),
      IsPosExp
        <$> (reserved "pos?" >> parens expression),
      -- Boolean literals
      TrueExp
        <$ reserved "true",
      FalseExp
        <$ reserved "false",
      -- Integer literal
      ConstExp
        <$> integer,
      -- Variable reference
      VarExp
        <$> identifier
    ]

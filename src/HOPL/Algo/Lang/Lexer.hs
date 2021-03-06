{-
 -  HOPL/MUTABLE_PAIRS/Lang/Lexer.hs
 -
 -  Reference implementation of the toy language MUTABLE_PAIRS from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the lexical specification for MUTABLE_PAIRS.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.Algo.Lang.Lexer where

import Text.Parsec ((<|>))
import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import qualified Data.Bool as Tok

mutablePairsLexer :: Tok.TokenParser ()
mutablePairsLexer =
  Tok.makeTokenParser $ mutablePairsDef

mutablePairsDef =
  emptyDef
    { Tok.commentLine = "%",
      Tok.identStart = letter,
      Tok.identLetter = alphaNum <|> oneOf "_-?",
      Tok.reservedOpNames = ["=", "-","+","%","/","^","*",">","<","<=",">=","&&","||"],
      Tok.reservedNames =

        [ "true",
          "false",
          "list",
          "let",
          "in",
          "if",
          "then",
          "else",
          "zero?",
          "proc",
          "letrec",
          "set",
          "begin",
          "end",
          "newpair",
          "left",
          "right",
          "setleft",
          "setright",
          "emptylist"
        ]
    }

integer :: Parser Integer
integer = Tok.integer mutablePairsLexer

symbol :: String -> Parser String
symbol = Tok.symbol mutablePairsLexer

parens :: Parser a -> Parser a
parens = Tok.parens mutablePairsLexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep mutablePairsLexer

identifier :: Parser String
identifier = Tok.identifier mutablePairsLexer


reserved :: String -> Parser ()
reserved = Tok.reserved mutablePairsLexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp mutablePairsLexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace mutablePairsLexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral mutablePairsLexer


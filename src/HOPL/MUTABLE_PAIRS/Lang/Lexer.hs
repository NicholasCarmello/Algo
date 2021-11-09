{-
 -  HOPL/MUTABLE_PAIRS/Lang/Lexer.hs
 -
 -  Reference implementation of the toy language HOPL.LET by Mitchell Wand.
 -  This module provides the lexical specification for LET.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.MUTABLE_PAIRS.Lang.Lexer where

import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

letLexer :: Tok.TokenParser ()
letLexer =
  Tok.makeTokenParser $ letDef

letDef =
  emptyDef
    { Tok.commentLine = "%",
      Tok.reservedOpNames = ["=", "-"],
      Tok.reservedNames =
        [ "let",
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
          "setright"
        ]
    }

integer :: Parser Integer
integer = Tok.integer letLexer

symbol :: String -> Parser String
symbol = Tok.symbol letLexer

parens :: Parser a -> Parser a
parens = Tok.parens letLexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep letLexer

identifier :: Parser String
identifier = Tok.identifier letLexer

reserved :: String -> Parser ()
reserved = Tok.reserved letLexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp letLexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace letLexer

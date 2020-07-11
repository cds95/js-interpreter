{-# LANGUAGE FlexibleContexts #-}
module JS.Parser where

import JS.Core

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.Parsec.Prim hiding (State, try)
import Control.Monad

type Parser = ParsecT String () Identity

--- ### Lexers

parseWith :: Parser a -> String -> Either ParseError a
parseWith p = parse p ""

digitP :: Parser Char
digitP = oneOf ['0'..'9']

digitsP :: Parser String
digitsP = many1 digitP

maybeSpaceP :: Parser String
maybeSpaceP = many $ oneOf " \n\t"

spaceP :: Parser String
spaceP = many1 $ oneOf " \n\t"

idP :: Parser String
idP = liftM2 (:) identFirst (many identRest)
  where identFirst = oneOf $ "-*+/:?><=!" ++ ['a'..'z'] ++ ['A'..'Z']
        identRest  = identFirst <|> digitP

--- ### Value parsers

symP :: Parser Val
symP = Symbol <$> idP

numP :: Parser Val
numP = Number . read <$> digitsP

boolP :: Parser Val
boolP = char '#' >> Boolean <$> boolLitP
  where boolLitP = const True <$> char 't'
               <|> const False <$> char 'f'
               <?> "a boolean (#f or #t)"

rawExprP :: Parser Val
rawExprP = numP
       <|> symP
       <|> boolP
       <?> "a value"

exprP :: Parser Val
exprP = between maybeSpaceP maybeSpaceP rawExprP <* eof

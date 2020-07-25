{-# LANGUAGE FlexibleContexts #-}
module JS.Parser where

import JS.Core

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.Parsec.Prim hiding (State, try)
import Text.Read
import Control.Monad

type Parser = ParsecT String () Identity

parseToExp :: [String] -> Exp 
parseToExp ("const":varName:"=":xs) = ConstAssignExp varName (parseToExp xs)
parseToExp ("let":varName:"=":xs) = LetExp varName (parseToExp xs)
parseToExp [i] = 
  case i of 
    "true" -> ConstExp (ConstVal (Boolean True))
    "false" -> ConstExp (ConstVal (Boolean False))
    _ -> 
      case readMaybe i of 
        (Just a) -> ConstExp (ConstVal (Num a))
        _ -> VarExp i
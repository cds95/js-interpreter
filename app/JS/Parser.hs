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
parseToExp i@("if":xs) = parseIf i
parseToExp [i] = 
  case i of 
    "true" -> ConstExp (ConstVal (Boolean True))
    "false" -> ConstExp (ConstVal (Boolean False))
    _ -> 
      case readMaybe i of 
        (Just a) -> ConstExp (ConstVal (Num a))
        _ -> VarExp i
parseToExp f@("function":xs) = parseFunction f
parseToExp (x:op:y) = BinOpExp op (parseToExp [x]) (parseToExp y)

parseFunction :: [String] -> Exp
parseFunction ("function":fnName:rest) = 
  let (fnParams, fnStatementRest) = parseFnParams rest 
      (fnBody, _) = getExpList fnStatementRest
  in FunExp fnName fnParams fnBody

parseFnParams :: [String] -> ([String], [String])
parseFnParams ("(":xs) = aux xs []
  where aux (")":rest) params = (params, rest)
        aux (",":rest) params = aux rest params
        aux (p:pRest) params = aux pRest (params ++ [p])

parseIf ("if":xs) = 
  let (condExp, restOfIf) = getCondExp xs
      (ifBody, elseStatement) = getExpList restOfIf 
  in case elseStatement of 
    [] -> IfExp condExp ifBody []
    _ -> IfExp condExp ifBody elseBody
      where (elseBody, _) = getExpList elseStatement

getCondExp :: [String] -> (Exp, [String]) 
getCondExp xs = aux xs []
  where aux (")":xs) condition = (parseToExp condition, xs)
        aux ("(":xs) condition = aux xs condition
        aux (x:xs) condition = aux xs (condition ++ [x])

getExpList :: [String] -> ([Exp], [String])
getExpList xs = aux xs [] []
  where aux ("{":xs) currStatement expList = aux xs currStatement expList
        aux ("}":xs) currStatement expList = (expList, xs)
        aux (";":xs) currStatement expList = aux xs [] (expList ++ [parseToExp currStatement])
        aux (x:xs) currStatement expList = aux xs (currStatement ++ [x]) expList
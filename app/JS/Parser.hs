{-# LANGUAGE FlexibleContexts #-}
module JS.Parser where

import JS.Core

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.Parsec.Prim hiding (State, try)
import Text.Read
import Control.Monad
import Data.HashMap.Strict as H (HashMap, lookup, fromList)

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
        (Just a) -> ConstExp (LetVal (Num a))
        _ -> VarExp i
parseToExp f@("function":xs) = parseFunction f
parseToExp f@("print":xs) = parsePrint f
parseToExp f@("for":xs) = parseFor f
parseToExp f@(x:"(":xs) = parseAppExp f
parseToExp (x:op:y) = BinOpExp op (parseToExp [x]) (parseToExp y)

loopOperations = H.fromList [
                    ("<", (\x y -> y < x)),
                    ("<=", (\x y -> y <= x)),
                    (">", (\x y -> y > x)),
                    (">=", (\x y -> y >= x))
                ]

parseFor :: [String] -> Exp 
parseFor ("for":xs) = 
  let (startIdx, restOne) = getStartIdx xs
      (shouldContinueLoop, restTwo) = getShouldContinueLoop restOne
      (idxUpdateFn, (")":restThree)) = getIdxUpdateFn restTwo
      (loopBody, _) = getExpList restThree
  in ForExp startIdx shouldContinueLoop idxUpdateFn loopBody

getStartIdx ("(":"var":x:"=":startIdx:";":rest) = ((read startIdx), rest)

getShouldContinueLoop :: [String] -> (Integer -> Bool, [String])
getShouldContinueLoop (x:op:endIdx:";":rest) =
  case H.lookup op loopOperations of 
    Just loopFn -> ((loopFn (read endIdx)), rest)

getIdxUpdateFn :: [String] -> (Integer -> Integer, [String])
getIdxUpdateFn (x:"++":rest) = ((\x -> x + 1), rest)
getIdxUpdateFn (x:"--":rest) = ((\x -> x - 1), rest)

parsePrint :: [String] -> Exp
parsePrint ("print":"(":x:_) = PrintExp x

parseFunction :: [String] -> Exp
parseFunction ("function":fnName:rest) = 
  let (fnParams, fnStatementRest) = parseFnParams rest 
      (fnBody, _) = getExpList fnStatementRest
  in FunExp fnName fnParams fnBody

parseFnParams :: [String] -> ([String], [String])
parseFnParams xx = aux xx []
  where aux ("(":xs) params = aux xs params
        aux (")":rest) params = (params, rest)
        aux (",":rest) params = aux rest params
        aux (p:pRest) params = aux pRest (params ++ [p])

parseAppExp :: [String] -> Exp
parseAppExp (fnName:rest)= 
  let params = getFnAppParams rest 
  in AppExp fnName params

getFnAppParams :: [String] -> [Exp]
getFnAppParams xx = aux xx []
  where aux [] res = res
        aux (")":[]) res = res 
        aux (",":rest) res = aux rest res 
        aux ("(":rest) res = aux rest res 
        aux ("true":rest) res = aux rest ([BoolExp (BoolVal True)] ++ res)
        aux ("false":rest) res = aux rest ([BoolExp (BoolVal False)] ++ res)
        aux (x:rest) res = aux rest ([IntExp (IntVal (read x))] ++ res)

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
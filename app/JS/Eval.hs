module JS.Eval where 
import JS.Core
import JS.Runtime

import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Control.Monad.Except
import Control.Monad.State

eval :: Exp -> Env -> JSOutput 
eval (VarExp varName) env = 
    case H.lookup varName env of 
        Just a -> (env, a)
        Nothing -> (env, Nil)

eval (ConstExp val) e = (e, val)

eval (LetExp varName exp) env = assignVariableToEnv varName exp env 

eval (ConstAssignExp varName exp) env = assignVariableToEnv varName exp env

eval (BinOpExp operator e1 e2) env = 
    let (_, valOne) = eval e1 env 
        (_, valTwo) = eval e2 env 
    in case H.lookup operator runtimeOperations of 
        (Just f) -> (env, result)
            where result = liftOp f valOne valTwo
        Nothing -> (env, Error (operator ++ " is not a valid operation"))

eval (IfExp condExp ifBodyExps elseExps) env = 
    let ifEnv = env 
        (_, value) = eval condExp env 
    in case value of
        (LetVal (Boolean True)) -> handleIfBlock ifBodyExps ifEnv
        (ConstVal (Boolean True)) -> handleIfBlock ifBodyExps ifEnv
        _ -> handleIfBlock elseExps ifEnv

handleIfBlock :: [Exp] -> Env -> JSOutput
handleIfBlock ifBodyExps env = aux ifBodyExps env 
    where aux [] env = (env, Nil)
          aux (x:xs) env = aux xs newEnv
            where (newEnv, _) = eval x env 

assignVariableToEnv varName exp env = 
    case H.lookup varName env of 
        (Just (ConstVal _)) -> (env, (Error "Cannot reassign constant variable"))
        _ -> 
            let (_, evaledVar) = eval exp env 
                newEnv = H.insert varName evaledVar env 
            in (newEnv, evaledVar)

    
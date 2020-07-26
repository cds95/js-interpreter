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

assignVariableToEnv varName exp env = 
    case H.lookup varName env of 
        (Just (ConstVal _)) -> (env, (Error "Cannot reassign constant variable"))
        _ -> 
            let (_, evaledVar) = eval exp env 
                newEnv = H.insert varName evaledVar env 
            in (newEnv, evaledVar)

    
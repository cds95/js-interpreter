module JS.Eval where 
import JS.Core

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

assignVariableToEnv varName exp env = 
    case H.lookup varName env of 
        (Just (ConstVal _)) -> (env, (Error "Cannot reassign constant variable"))
        _ -> 
            let (_, evaledVar) = eval exp env 
                newEnv = H.insert varName evaledVar env 
            in (newEnv, evaledVar)

    
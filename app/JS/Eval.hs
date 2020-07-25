module JS.Eval where 
import JS.Core

import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Control.Monad.Except
import Control.Monad.State

envLookupHelper sym env = 
    case H.lookup sym env of 
        Just x -> x
        _ -> case reads sym of 
            [(i , "")] -> Number i

eval :: Exp -> Env -> JSOutput 
eval (VarExp varName) env = 
    case H.lookup varName env of 
        Just a -> (env, a)
        Nothing -> (env, Nil)

    
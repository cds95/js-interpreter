module JS.Eval where 
import JS.Core

import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Control.Monad.Except
import Control.Monad.State

eval :: Val -> EvalState Val
eval v@(Number _) = return v
eval v@(Boolean _) = return v 

eval (Symbol s) = do 
    env <- get 
    case H.lookup s env of
        Nothing -> throwError (UndefinedError s)
        Just a -> return a

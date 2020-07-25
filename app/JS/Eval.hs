module JS.Eval where 
import JS.Core
import JS.Compile

import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Control.Monad.Except
import Control.Monad.State

envLookupHelper sym env = 
    case H.lookup sym env of 
        Just x -> x
        _ -> case reads sym of 
            [(i , "")] -> Number i

eval :: [String] -> JSState -> JSState
eval (w:ws) s@(is, env, o) = 
    case envLookupHelper w env of 
        (Number a) -> eval ws ((a:is), env, o)
        Define -> compile (w:ws) s

eval [] s@(is, env, o) = (is, env, ("ok":o))

    
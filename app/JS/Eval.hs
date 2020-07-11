module JS.Eval where 
import JS.Core
import JS.Compile

import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Control.Monad.Except
import Control.Monad.State

envLookupHelper sym env = 
    case H.lookup sym env of 
        Just x -> x

eval :: [String] -> JSState -> JSState
eval (w:ws) s@(is, env, o) = 
    case envLookupHelper w env of 
        Define -> compile (w:ws) s

    
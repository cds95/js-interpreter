module JS.Compile where 
import JS.Core
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)

compileLet :: JSState -> JSState
compileLet (is, env, o) = (is, env, ["let"])

compileActions :: H.HashMap String (JSState -> JSState)
compileActions = H.fromList [("let", compileLet)]

compile :: [String] -> JSState -> JSState
compile (w:ws) s@(is, env, o) = 
    case H.lookup w compileActions of 
        Just action -> action s 
        Nothing -> ([], env, ("error": o))
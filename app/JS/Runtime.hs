module JS.Runtime where 
import JS.Core
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import JS.Compile

runtime :: JSState
initialEnv = H.fromList [("let", Define)]

compileActions = H.fromList [("let", compileLet)]
runtime = ([], initialEnv, [])
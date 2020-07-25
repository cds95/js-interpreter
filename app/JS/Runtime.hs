module JS.Runtime where 
import JS.Core
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)

initialEnv :: Env
initialEnv = H.fromList []
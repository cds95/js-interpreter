module JS.Core where 
    
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)

type Env = H.HashMap String String 
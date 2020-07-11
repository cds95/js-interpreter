module JS.Core where 
    
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Control.Monad.Except
import Control.Monad.State

type Env = H.HashMap String Val
data Val = Number Int |
           Boolean Bool |
           JSString String |
           Nil | 
           Symbol String 

instance Show Val where 
       show (Number a) = show a
       show (Boolean a) = show a

data Diagnostic = UnimplementedError String |
                  UndefinedError String

instance Show Diagnostic where 
       show (UnimplementedError a) = a ++ " is unimplemented"
       show (UndefinedError a) = a ++ " is undefined"

type EvalState a = StateT Env (Except Diagnostic) a
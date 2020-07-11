module JS.Core where 
    
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Control.Monad.Except
import Control.Monad.State

type Env = H.HashMap String String 
data Val = Number Int |
           Boolean Bool |
           Nil | 
           Symbol String 

instance Show Val where 
       show (Number a) = show a

data Diagnostic = UnimplementedError String
    deriving (Show)

type EvalState a = StateT Env (Except Diagnostic) a
module JS.Core where 
    
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Control.Monad.Except
import Control.Monad.State

type Env = H.HashMap String Val
data Val = Number Integer |
           Boolean Bool |
           Nil

data Exp = LetExp String Exp |
           VarExp String |
           ConstExp Val

instance Show Val where 
       show (Number a) = show a
       show (Boolean a) = show a
       show _ = "ok"

data Diagnostic = UnimplementedError String |
                  UndefinedError String |
                  AssigningToConstantError String 

instance Show Diagnostic where 
       show (UnimplementedError a) = a ++ " is unimplemented"
       show (UndefinedError a) = a ++ " is undefined"

type JSOutput = (Env, Val)

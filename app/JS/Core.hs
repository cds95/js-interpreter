module JS.Core where 
    
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Control.Monad.Except
import Control.Monad.State

type Env = H.HashMap String Val
data Primitive = Num Integer | Boolean Bool deriving Show
data Val = LetVal Primitive |
           ConstVal Primitive |
           Nil | 
           Error String
           deriving Show

data Exp = LetExp String Exp |
           ConstAssignExp String Exp |
           VarExp String |
           ConstExp Val |
           BinOp String Exp Exp 

data Diagnostic = UnimplementedError String |
                  UndefinedError String |
                  AssigningToConstantError String 

instance Show Diagnostic where 
       show (UnimplementedError a) = a ++ " is unimplemented"
       show (UndefinedError a) = a ++ " is undefined"

type JSOutput = (Env, Val)

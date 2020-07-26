module JS.Core where 
    
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Control.Monad.Except
import Control.Monad.State

type Env = H.HashMap String Val
type PrimitiveOp = Primitive -> Primitive -> Primitive
type RuntimeOperationsEnv = H.HashMap String PrimitiveOp
data Primitive = Num Integer | Boolean Bool 

instance Show Primitive where 
       show (Num a) = show a 
       show (Boolean a) = show a 

data Val = LetVal Primitive |
           ConstVal Primitive |
           Nil | 
           Error String |
           Output String

instance Show Val where 
       show Nil = "null"
       show (ConstVal a) = show a 
       show (LetVal a) = show a 
       show (Error a) = a 
       show (Output a) = a

data Exp = LetExp String Exp |
           ConstAssignExp String Exp |
           VarExp String |
           ConstExp Val |
           BinOpExp String Exp Exp |
           IfExp Exp [Exp] [Exp] 
           deriving Show

data Diagnostic = UnimplementedError String |
                  UndefinedError String |
                  AssigningToConstantError String 

instance Show Diagnostic where 
       show (UnimplementedError a) = a ++ " is unimplemented"
       show (UndefinedError a) = a ++ " is undefined"

type JSOutput = (Env, Val)

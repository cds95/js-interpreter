module JS.Eval where 
import JS.Core

eval :: Val -> EvalState Val
eval v@(Number _) = return v
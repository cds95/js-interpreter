module JS.Eval where 
import JS.Core
import JS.Runtime

import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.Except
import Control.Monad.State

eval :: Exp -> Env -> JSOutput 
eval (VarExp varName) env = 
    case H.lookup varName env of 
        Just a -> (env, [a])
        Nothing -> (env, [Nil])

eval (IntExp v) e = (e, [v])
eval (BoolExp v) e = (e, [v])

eval (ConstExp val) e = (e, [val])

eval (LetExp varName exp) env = assignVariableToEnv varName exp env LetVal

eval (ConstAssignExp varName exp) env = assignVariableToEnv varName exp env ConstVal

eval (BinOpExp operator e1 e2) env = 
    let (_, [valOne]) = eval e1 env 
        (_, [valTwo]) = eval e2 env 
    in case H.lookup operator runtimeOperations of 
        (Just f) -> (env, [result])
            where result = liftOp f valOne valTwo
        Nothing -> (env, [Error (operator ++ " is not a valid operation")])

eval (IfExp condExp ifBodyExps elseExps) env = 
    let ifEnv = env 
        (_, [value]) = eval condExp env 
    in case value of
        (LetVal (Boolean True)) -> evalMultipleExp ifBodyExps (ifEnv, [Nil])
        (ConstVal (Boolean True)) -> evalMultipleExp ifBodyExps (ifEnv, [Nil])
        _ -> evalMultipleExp elseExps (ifEnv, [Nil])

eval f@(ForExp currIdx loopHasEnded idxUpdater exps) env = forLoopHelper f env []

eval (FunExp fnName fnParams fnBody) env = 
    let closure = CloVal fnParams fnBody env 
        newEnv = H.insert fnName closure env 
    in (newEnv, [Nil])

eval (AppExp fnName params) env =
    let fn = H.lookup fnName env 
    in case fn of 
        Nothing -> (env, [(Error (fnName ++ " is not defined"))])
        Just (CloVal fnParams fnBody clenv) -> 
            let fnEnv = getFnEnv fnParams params clenv 
                (_, val) = evalMultipleExp fnBody (fnEnv, [Nil])
            in (env, val)

eval (PrintExp var) env = 
    let envVar = H.lookup var env 
    in case envVar of 
        Nothing -> (env, [(Error "undefined")])
        Just foundVal -> (env, [foundVal]) 

eval w@(WhileExp whileExp whileBody) env = whileLoopHelper w env []

forLoopHelper (ForExp currIdx shouldContinueLoop idxUpdater exps) env res = 
    case shouldContinueLoop currIdx of
        False -> (env, res)
        _ -> 
            let (newEnv, bodyRes) = evalMultipleExp exps (env, res)
                updatedIdx = idxUpdater currIdx
            in forLoopHelper (ForExp updatedIdx shouldContinueLoop idxUpdater exps) newEnv bodyRes 

whileLoopHelper (WhileExp condExp exps) env res = 
    case eval condExp env of 
        (newEnv, [(ConstVal (Boolean False))]) -> (env, res)
        _ -> whileLoopHelper (WhileExp condExp exps) newEnv bodyRes
            where (newEnv, bodyRes) = evalMultipleExp exps (env, res)
            

getFnEnv :: [String] -> [Exp] -> Env -> Env
getFnEnv args params env = aux args params env 
    where aux [] [] env = env 
          aux (argOne:argRest) (pOne:pRest) env =
              let (_, [evaledParam]) = eval pOne env 
                  newEnv = H.insert argOne evaledParam env
              in aux argRest pRest newEnv

evalMultipleExp :: [Exp] -> JSOutput -> JSOutput
evalMultipleExp expList jsOutput = aux expList jsOutput 
    where aux [] output = output
          aux (x:xs) jsOutput@(env, val) = aux xs (newEnv, (val ++ evaledVal))
            where (newEnv, evaledVal) = eval x env 

assignVariableToEnv varName exp env valConstructor = 
    case H.lookup varName env of 
        (Just (ConstVal _)) -> (env, [(Error "Cannot reassign constant variable")])
        _ -> 
            let (_, [evaledArg]) = eval exp env 
            in case evaledArg of 
                (ConstVal a) -> assignVariableToEnvHelper a varName valConstructor env 
                (LetVal a) -> assignVariableToEnvHelper a varName valConstructor env 
                (IntVal a) -> assignVariableToEnvHelper (Num a) varName valConstructor env 

assignVariableToEnvHelper a varName valConstructor env =  
    let newEnv = H.insert varName (valConstructor a) env 
    in (newEnv, [Nil])

    
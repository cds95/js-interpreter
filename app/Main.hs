module Main where 
import JS.Core
import JS.Runtime 
import JS.Parser 
import JS.Eval
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

repl :: Env -> IO ()
repl env = do 
    putStr "js> "
    l <- getLine
    let (newEnv, output) = eval (parseToExp (words l)) env 
    printOutput output
    repl newEnv

printOutput :: [Val] -> IO ()
printOutput [] = return ()
printOutput ((Nil):xs) = do 
     result <- printOutput xs 
     return result 
printOutput ((BreakVal):xs) = printOutput []
printOutput (x:xs) = do 
    print x 
    result <- printOutput xs 
    return result 

addSpacing :: [Char] -> [Char]
addSpacing xx = aux xx []
    where aux [] res = res 
          aux [x] res = res ++ [x]
          aux (x:xs) res = aux xs (res ++ [x] ++ " ")
          
main :: IO () 
main = repl initialEnv
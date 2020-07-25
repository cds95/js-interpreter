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
    print output
    print newEnv
    repl newEnv

main :: IO () 
main = repl initialEnv
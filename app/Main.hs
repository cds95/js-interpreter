module Main where 
import JS.Core
import JS.Runtime 
import JS.Parser 
import JS.Eval
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

repl :: JSState -> IO ()
repl env = do 
    putStr "js> "
    l <- getLine
    let (is, newEnv, output) = eval (words l) env 
    print output
    repl (is, newEnv, output)

main :: IO () 
main = repl runtime
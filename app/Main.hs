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
    case parse exprP "Expression" l of 
        Left err -> print err 
        Right exp -> 
            case runExcept $ runStateT (eval exp) env of
                Left err -> print err 
                Right (res, newEnv) -> do 
                    print res 
                    repl newEnv
    repl env

main :: IO () 
main = repl runtime
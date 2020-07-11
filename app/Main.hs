module Main where 
import JS.Core
import JS.Runtime 
import JS.Parser 
import Text.ParserCombinators.Parsec hiding (Parser, State)

repl :: Env -> IO ()
repl env = do 
    putStr "js> "
    l <- getLine
    case parse exprP "Expression" l of 
        Left err -> print err 
        Right exp -> do 
            print exp
    repl env

main :: IO () 
main = repl runtime
module Main where 
import JS.Core

import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)

repl :: Env -> IO ()
repl env = do 
    putStr "js> "
    l <- getLine
    print l
    repl env

runtime :: Env 
runtime = H.fromList []
main :: IO () 
main = repl runtime
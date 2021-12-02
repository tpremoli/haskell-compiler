module Main where

import System.Environment
import Compiler
import Machine
import Interpreter
import Text.Read

-- Task 3.4
main :: IO ()
main = do
    x <- getArgs
    print (ccomp (readCom (x !! 0)))

readCom :: String -> Com
readCom s = read s

-- example input:
-- stack exec coursework-exe "Assign \"x\" (Plus (N 5) (N 3))"
module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map
import Machine

-- Task 2.1
data AExp = N Int
        | V String
        | Plus AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.2
aval :: AExp -> State -> Val
aval = undefined 

-- Task 2.1
data BExp = Bc Bool 
        | Not Bool
        | And Bool Bool
        | Less AExp AExp
    Bundefined
    deriving (Eq, Read, Show)

--TODO Task 2.3
bval :: BExp -> State -> Bool
bval = undefined 

-- Task 2.1
data Com = Assign String AExp
        | Seq Com Com
        | If BExp Com Com
        | While BExp Com
        | SKIP
    deriving (Eq, Read, Show)

--TODO Task 2.4
eval :: Com -> State -> State
eval = undefined
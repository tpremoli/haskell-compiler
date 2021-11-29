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

-- Task 2.2
aval :: AExp -> State -> Val
aval (N x) s                = x
aval (V x) s                = Data.Map.findWithDefault 0 x s
aval (Plus x y) s           = (aval x s) + (aval y s)

-- Task 2.1
data BExp = Bc Bool 
        | Not Bool
        | And Bool Bool
        | Less AExp AExp
    deriving (Eq, Read, Show)

-- Task 2.3
bval :: BExp -> State -> Bool
bval (Bc x) s               = x
bval (Not x) s              = if x == True then False else True
bval (And True True) s      = True
bval (And _ _ ) s           = False
bval (Less x y) s           = if (aval x s) < (aval y s) then True else False   

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
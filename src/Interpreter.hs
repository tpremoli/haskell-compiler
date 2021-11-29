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
        | Not BExp
        | And BExp BExp
        | Less AExp AExp
    deriving (Eq, Read, Show)

-- Task 2.3
bval :: BExp -> State -> Bool
bval (Bc x) s       = x
bval (Not x) s      = if (bval x s) then False else True
bval (And x y) s    = if (bval x s) == (bval y s) && (bval y s) == True then True else False
bval (Less x y) s   = if (aval x s) < (aval y s) then True else False   

-- Task 2.1
data Com = Assign String AExp
        | Seq Com Com
        | If BExp Com Com
        | While BExp Com
        | SKIP
    deriving (Eq, Read, Show)

-- Task 2.4
eval :: Com -> State -> State
eval (Assign v x) s     = (Data.Map.insert v (aval x s) s)
eval (Seq c1 c2) s      = eval c2 (eval c1 s)
eval (If b c1 c2) s     = if (bval b s) 
                                then eval c1 s
                                else eval c2 s
eval (While b c) s      = if (bval b s)
                            then    let r = (eval c s)
                                    in eval (While b c) r
                            else s
eval SKIP s             = s
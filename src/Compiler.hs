module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter

-- Task 3.1
acomp :: AExp -> [Instr]
acomp (N x)         =   [LOADI x]
acomp (V x)         =   [LOAD x]
acomp (Plus x y)    =   (acomp x) ++ (acomp y) ++ [ADD]

--TODO Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc x) y z            =   if x == y
                                    then [JMP z]
                                    else []
bcomp (Not x) y z           =   if x /= (Bc y)
                                    then [JMP z]
                                    else []
bcomp (And x1 x2) True z    =   let r2 = bcomp x2 True z in
                                    if r2 == [] 
                                        then []
                                        else let r1 = bcomp x1 True z in
                                            if r1 == [] 
                                                then [JMP (length r2)] ++ r2 
                                                else r1
bcomp (And x1 x2) y z       |   x1 == Bc False = let r1 = (bcomp x2 y z) in [JMP ((length r1) + z)] ++ (bcomp x2 y z)
                            |   otherwise = (bcomp x1 y z) ++ (bcomp x2 y z)
bcomp (Less x1 x2) True z   =   (acomp x1) ++ (acomp x2) ++ [JMPLESS z]
bcomp (Less x1 x2) False z  =   (acomp x1) ++ (acomp x2) ++ [JMPGE z]

--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp x  =  []

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
bcomp (And x1 x2) True z    =   if x1 == x2 && x2 == (Bc True) 
                                    then [JMP z] 
                                    else []
bcomp (And x1 x2) False z   =   if (x1 /= x2) || (x1 == x2 && x2 == (Bc False))
                                    then [JMP z] 
                                    else []
bcomp (Less x1 x2) True z   =   (acomp x1) ++ (acomp x2) ++ [JMPLESS z]
bcomp (Less x1 x2) False z  =   (acomp x1) ++ (acomp x2) ++ [JMPGE z]

--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp = undefined
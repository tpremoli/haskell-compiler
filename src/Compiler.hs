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

-- Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc x) y z            =   if x == y       -- simple
                                    then [JMP z]
                                    else []
bcomp (Not x) y z           =   if x /= (Bc y)  -- simple
                                    then [JMP z]
                                    else []
bcomp (And x1 x2) y z       =   let r2 = bcomp x2 y z   -- compile both statements.
                                    r1 = bcomp x1 False (length (r2) + 
                                            (if y == True then 0 else z))
                                in r1 ++ r2 -- connect both statements
bcomp (Less x1 x2) True z   =   (acomp x1) ++ (acomp x2) ++ [JMPLESS z] -- true meansJMPLSES
bcomp (Less x1 x2) False z  =   (acomp x1) ++ (acomp x2) ++ [JMPGE z]   -- false means JMPGE

--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp (Assign n v)          =   (acomp v) ++ [STORE n]
ccomp (Seq x1 x2)           =   ccomp x1 ++ ccomp x2

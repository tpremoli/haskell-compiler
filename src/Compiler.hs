module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter

--TODO Task 3.1
acomp :: AExp -> [Instr]
acomp (N x)         =   [LOADI x]
acomp (V x)         =   [LOAD x]
acomp (Plus x y)    =   (acomp x) ++ (acomp y) ++ [ADD]

--TODO Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp = undefined

--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp = undefined
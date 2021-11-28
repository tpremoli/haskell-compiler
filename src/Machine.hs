module Machine
(      
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map

-- Task 1.1 Define a type Vname to model variable names as strings.
type Vname = String

-- Task 1.2 Define a type Val to model variable values.
type Val = Int

-- Task 1.3 Define a type State for states which maps variable names to values.
type State = [(Vname, Val)]
addState :: State -> Vname -> Val -> State
addState s n v
        | n `elem` (map fst s) = 
          ([c | c <- s, fst c /= n]) ++ [(n,v)]
        | otherwise = s ++ [(n,v)]


-- checkif vname n is in state s
-- gets zeroth element of a list of the second value in the tuple where the first matches Vname n
-- roundabout way of just getting the tuple where the first value matches the input but haskell is hard
-- i love spaghetti code
getVar :: State -> Vname -> Val
getVar s n
        | n `elem` (map fst s) = 
          (map snd [c | c <- s, fst c == n]) !! 0 
        | otherwise = 0 -- otherwise returns 0. not sure what else to do?


--TODO Task 1.4 Create a data type Instr which models all instructions supported by the machine.
data Instr = LOADI Val 
        | LOAD Vname
        | ADD Val Val
        | STORE Vname
        | JMP Int
        | JMPLESS Int Val Val
        | JMPGE Int Val Val
        deriving (Eq, Read, Show)


--TODO Task 1.5 Define a type Stack to model the stack.
type Stack = ()

--TODO Task 1.6 Define a type Config to model a configuration.
type Config = ()

--TODO Task 1.7 Define a function iexec :: Instr → Config → Config to execute a single instruction.
iexec :: Instr -> Config -> Config
iexec = undefined 

--TODO Task 1.8 Define a function exec :: [ Instr ] → Config → Config to execute a list of instructions.
exec :: [Instr] -> Config -> Config
exec = undefined


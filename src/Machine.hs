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
type Val = Integer

-- Task 1.3 Define a type State for states which maps variable names to values.
type State = (Map Vname Val)

--Task 1.4 Create a data type Instr which models all instructions supported by the machine.
data Instr = LOADI Val 
        | LOAD Vname
        | ADD
        | STORE Vname
        | JMP Integer
        | JMPLESS Integer
        | JMPGE Integer
        deriving (Eq, Read, Show)


-- Task 1.5 Define a type Stack to model the stack.
type Stack = [Integer]

-- Task 1.6 Define a type Config to model a configuration.
type Config = (Integer, State, Stack)

--TODO Task 1.7 Define a function iexec :: Instr → Config → Config to execute a single instruction.
iexec :: Instr -> Config -> Config
iexec = undefined 

--TODO Task 1.8 Define a function exec :: [ Instr ] → Config → Config to execute a list of instructions.
exec :: [Instr] -> Config -> Config
exec = undefined


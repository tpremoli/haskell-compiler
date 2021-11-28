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
type State = (Map Vname Val)

--Task 1.4 Create a data type Instr which models all instructions supported by the machine.
data Instr = LOADI Val 
        | LOAD Vname
        | ADD
        | STORE Vname
        | JMP Int
        | JMPLESS Int
        | JMPGE Int
        deriving (Eq, Read, Show)


-- Task 1.5 Define a type Stack to model the stack.
type Stack = [Int]

-- Task 1.6 Define a type Config to model a configuration.
type Config = (Int, State, Stack)

-- Task 1.7 Define a function iexec :: Instr → Config → Config to execute a single instruction.
iexec :: Instr -> Config -> Config
iexec (LOADI x) (a,b,c)         = (a + 1, b, ((x):c))
iexec (LOAD v) (a,b,c)          = let r = Data.Map.findWithDefault 0 v b
                                in (a + 1, b, ((r):c))
iexec (ADD) (a,b,c0:c1:c)       = (a + 1, b, ((c0+c1):c))
iexec (STORE v) (a,b,c0:c)      = (a + 1, (Data.Map.insert v c0 b ), c)
iexec (JMP i) (a,b,c)           = (a + i + 1, b, c)
iexec (JMPLESS i) (a,b,c0:c1:c) = if(c1<c0) then (a + i + 1, b, c) else (a + 1, b, c)
iexec (JMPGE i) (a,b,c0:c1:c)   = if(c1>=c0) then (a + i + 1, b, c) else (a + 1, b, c)

--TODO Task 1.8 Define a function exec :: [ Instr ] → Config → Config to execute a list of instructions.
exec :: [Instr] -> Config -> Config
exec (xs) (a,b,c) = if (length xs) > (a) then 
                        let q = (iexec (xs !! a) (a,b,c))
                        in exec xs q
                        else (a,b,c)



-- Some helpful test cases
-- main = do
--     print(iexec (LOADI 5) (0, empty, []) )
--     print(iexec (LOAD "v1") (0, fromList [("v1",5)], []) )
--     print(iexec ADD (0, empty, [5,6]) )
--     print(iexec (STORE "x") (0, empty, [5]) )
--     print(iexec (JMP 5) (0, empty, []) )
--     print(iexec (JMPLESS 5) (0, empty, [5,6]) )
--     print(iexec (JMPGE 5) (0, empty, [5,6]) )
--     print(exec [LOADI 1, LOADI 2, ADD] (0, empty, []))
--     print(exec [LOADI 1, STORE "v1", LOADI 2, STORE "v2"] (0, empty, []))
import qualified Data.Map as M
import Data.Char (isSpace)
import Text.Read (readMaybe)

-- value type: keep it polymorphic
type Stack a = [a]

-- dictionary: variables by name
type Dict a = M.Map String a

-- machine state parameterized by value type 'a'
data Machine a = Machine
  { stack :: Stack a
  , env   :: Dict a
  , ip    :: Int                 -- instruction pointer
  , out   :: [String]            -- output log
  } deriving (Show, Functor)

-- arithmetic and comparison operators are abstracted via a typeclass
class StackOps a where
  add  :: a -> a -> Either String a
  sub  :: a -> a -> Either String a
  mul  :: a -> a -> Either String a
  divS :: a -> a -> Either String a      -- safe division
  neg  :: a -> Either String a
  eqS  :: a -> a -> Either String Bool
  ltS  :: a -> a -> Either String Bool

-- default instance for Double
instance StackOps Double where
  add x y = Right (x + y)
  sub x y = Right (x - y)
  mul x y = Right (x * y)
  divS _ 0 = Left "Division by zero"
  divS x y = Right (x / y)
  neg x    = Right (-x)
  eqS x y  = Right (x == y)
  ltS x y  = Right (x < y)

-- Instruction set
data Instr a
  = Push a
  | Load String                 -- push variable value
  | Store String                -- pop to variable
  | Add | Sub | Mul | Div | Neg
  | Dup | Swap | Pop
  | Eq  | Lt
  | Jump Int                    -- absolute jump
  | JumpIfZero Int              -- pop; if == 0 then jump
  | JumpIfTrue Int              -- pop Bool-like: non-zero or True
  | Print                       -- pop and write to output
  deriving (Show, Eq)
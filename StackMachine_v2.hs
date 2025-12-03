import qualified Data.Map as M
import Data.Char (isSpace)
import Text.Read (readMaybe)

type Stack a = [a]
type Env a   = M.Map String a

-- machine state parameterized by value type 'a'
data Machine a = Machine
  { stack :: Stack a
  , env   :: Env a
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
  | Load String
  | Store String
  | Add | Sub | Mul | Div | Neg
  | Dup | Swap | Pop
  | Eq  | Lt
  | Jump Int
  | JumpIfZero Int
  | JumpIfTrue Int
  | Print
  deriving (Show, Eq)

-- one step execution
step :: (StackOps a, Eq a, Show a, Num a) => [Instr a] -> Machine a -> Either String (Machine a)
step prog m
  | ip m < 0 || ip m >= length prog = Right m
  | otherwise =
      case prog !! ip m of
        Push v   -> ok m{ stack = v : stack m, ip = ip m + 1 }
        Load k   -> case M.lookup k (env m) of
                      Just v -> ok m{ stack = v : stack m, ip = ip m + 1 }
                      Nothing -> err ("Unknown variable: " ++ k)
        Store k  -> case stack m of
                      (v:vs) -> ok m{ stack = vs, env = M.insert k v (env m), ip = ip m + 1 }
                      _      -> err "Store: empty stack"
        Add      -> bin add
        Sub      -> bin sub
        Mul      -> bin mul
        Div      -> bin divS
        Neg      -> una neg
        Dup      -> case stack m of
                      (v:_) -> ok m{ stack = v : stack m, ip = ip m + 1 }
                      _     -> err "Dup: empty stack"
        Swap     -> case stack m of
                      (x:y:zs) -> ok m{ stack = y:x:zs, ip = ip m + 1 }
                      _        -> err "Swap: need two items"
        Pop      -> case stack m of
                      (_:vs) -> ok m{ stack = vs, ip = ip m + 1 }
                      _      -> err "Pop: empty stack"
        Eq       -> cmp eqS
        Lt       -> cmp ltS
        Jump tgt -> ok m{ ip = tgt }
        JumpIfZero tgt ->
          case stack m of
            (v:vs) -> ok m{ stack = vs, ip = if isZero v then tgt else ip m + 1 }
            _      -> err "JumpIfZero: empty stack"
        JumpIfTrue tgt ->
          case stack m of
            (v:vs) -> ok m{ stack = vs, ip = if isTrue v then tgt else ip m + 1 }
            _      -> err "JumpIfTrue: empty stack"
        Print    ->
          case stack m of
            (v:vs) -> ok m{ stack = vs, out = out m ++ [show v], ip = ip m + 1 }
            _      -> err "Print: empty stack"
  where
    ok = Right
    err = Left

    bin f = case stack m of
      (y:x:zs) -> case f x y of
                    Right r -> ok m{ stack = r:zs, ip = ip m + 1 }
                    Left e  -> err e
      _        -> err "Binary op: need two items"

    una f = case stack m of
      (x:xs) -> case f x of
                  Right r -> ok m{ stack = r:xs, ip = ip m + 1 }
                  Left e  -> err e
      _      -> err "Unary op: empty stack"

    cmp f = case stack m of
      (y:x:zs) -> case f x y of
                    Right True  -> ok m{ stack = one:zs, ip = ip m + 1 }
                    Right False -> ok m{ stack = zero:zs, ip = ip m + 1 }
                    Left e      -> err e
      _        -> err "Compare: need two items"

    isZero v = v == zero
    isTrue v = v /= zero
    zero = 0
    one  = 1

-- run until stop
run :: (StackOps a, Eq a, Show a, Num a) => [Instr a] -> Machine a -> Either String (Machine a)
run prog m0 = loop m0
  where
    loop m = case step prog m of
      Right m' ->
        if ip m' < 0 || ip m' >= length prog
           then Right m'
           else loop m'
      Left e  -> Left e

-- Example program: if x < 10 then print x else print (x - 10)
exampleProg :: [Instr Double]
exampleProg =
  [ Load "x"        -- 0
  , Push 10         -- 1
  , Lt              -- 2
  , JumpIfTrue 9    -- 3   if x<10 jump to print branch
  , Load "x"        -- 4
  , Push 10         -- 5
  , Sub             -- 6
  , Print           -- 7
  , Jump 11         -- 8   skip else branch
  , Load "x"        -- 9
  , Print           -- 10
  ]

main :: IO ()
main = do
  let m0 = Machine { stack = [], env = M.fromList [("x", 12)], ip = 0, out = [] }
  case run exampleProg m0 of
    Left e   -> putStrLn ("Runtime error: " ++ e)
    Right mf -> do
      putStrLn ("Final stack: " ++ show (stack mf))
      putStrLn ("Final env:   " ++ show (env mf))
      putStrLn ("Output:      " ++ show (out mf))

import qualified Data.Map as M

data Expr
    = Const Double              -- константа
    | Var String                -- переменная
    | Unary String Expr         -- унарная операция (cos, sqrt)
    | Binary String Expr Expr   -- бинарная операция (+, -, *, /)
    deriving (Eq, Show)

toString :: Expr -> String
toString (Const c) = show c
toString (Var v) = v
toString (Unary op e) = op ++ "(" ++ toString e ++ ")"
toString (Binary op l r) = "(" ++ toString l ++ " " ++ op ++ " " ++ toString r ++ ")"

eval :: M.Map String Double -> Expr -> Double
eval _ (Const c) = c
eval dict (Var v) = dict M.! v
eval dict (Unary "cos" e) = cos (eval dict e)
eval dict (Unary "sqrt" e) = sqrt (eval dict e)
eval dict (Unary "sin" e) = sin (eval dict e)
eval dict (Binary "+" l r) = eval dict l + eval dict r
eval dict (Binary "-" l r) = eval dict l - eval dict r
eval dict (Binary "*" l r) = eval dict l * eval dict r
eval dict (Binary "/" l r) = eval dict l / eval dict r

simplify :: Expr -> Expr
simplify (Binary "+" (Const 0) e) = simplify e
simplify (Binary "+" e (Const 0)) = simplify e
simplify (Binary "*" (Const 1) e) = simplify e
simplify (Binary "*" e (Const 1)) = simplify e
simplify (Binary "*" (Const 0) _) = Const 0
simplify (Binary "*" _ (Const 0)) = Const 0
simplify (Binary "-" e1 e2)
    | e1 == e2 = Const 0
simplify (Binary op l r) =
    let l' = simplify l
        r' = simplify r
    in case (l', r') of
        (Const a, Const b) -> Const (eval M.empty (Binary op (Const a) (Const b)))
        _ -> Binary op l' r'
simplify (Unary op e) =
    let e' = simplify e
    in case e' of
        Const a -> Const (eval M.empty (Unary op (Const a)))
        _       -> Unary op e'
simplify e = e

simplifyFix :: Expr -> Expr -- Это решение проблемы, когда после успешного упрощения какой-либо операции 
-- мы непытаемся заново упростить выражение содержащее одним из параметров уже упрощенное выражение
simplifyFix e =
    let e' = simplify e
    in if e' == e then e else simplifyFix e'

diff :: String -> Expr -> Expr
diff _ (Const _) = Const 0
diff v (Var x) = if x == v then Const 1 else Const 0
diff v (Binary "+" l r) = Binary "+" (diff v l) (diff v r)
diff v (Binary "-" l r) = Binary "-" (diff v l) (diff v r)
diff v (Binary "*" l r) =
    Binary "+"
        (Binary "*" (diff v l) r)
        (Binary "*" l (diff v r))
diff v (Binary "/" l r) =
    Binary "/"
        (Binary "-"
            (Binary "*" (diff v l) r)
            (Binary "*" l (diff v r)))
        (Binary "*" r r)
diff v (Unary "sin" e) =
    Binary "*" (Unary "cos" e) (diff v e)
diff v (Unary "cos" e) =
    Binary "*" (Binary "-" (Const 0) (Unary "sin" e)) (diff v e)
diff v (Unary "sqrt" e) =
    Binary "/"
        (diff v e)
        (Binary "*" (Const 2) (Unary "sqrt" e))
diff _ e = error ("Differentiation not defined for: " ++ show e)

prettyTree :: Expr -> String
prettyTree expr = go "" True expr
  where
    go :: String -> Bool -> Expr -> String
    go prefix isTail (Const c) =
        prefix ++ (if isTail then "└── " else "├── ") ++ show c ++ "\n"
    go prefix isTail (Var v) =
        prefix ++ (if isTail then "└── " else "├── ") ++ v ++ "\n"
    go prefix isTail (Unary op e) =
        prefix ++ (if isTail then "└── " else "├── ") ++ op ++ "\n"
        ++ go (prefix ++ (if isTail then "    " else "│   ")) True e
    go prefix isTail (Binary op l r) =
        prefix ++ (if isTail then "└── " else "├── ") ++ op ++ "\n"
        ++ go (prefix ++ (if isTail then "    " else "│   ")) False l
        ++ go (prefix ++ (if isTail then "    " else "│   ")) True r

expr :: Expr
expr = Binary "*" (Var "x") (Binary "-" (Var "x") (Var "x"))

main :: IO ()
main = do
    putStrLn $ "Expr: " ++ toString expr
    putStrLn (prettyTree expr)
    putStrLn $ "Simplified: " ++ toString (simplify expr)
    putStrLn $ "Simplified(fix): " ++ toString (simplifyFix expr)
    putStrLn $ "Eval at x=2: " ++ show (eval (M.fromList [("x",2)]) expr)
    putStrLn $ "Derivative wrt x: " ++ toString (diff "x" expr)
    putStrLn $ "Derivative wrt x(Simplified): " ++ toString (simplifyFix $ diff "x" expr)

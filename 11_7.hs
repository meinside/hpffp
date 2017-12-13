data Expr
  = Lit Integer
  | Add Expr
        Expr
  deriving (Show)

eval :: Expr -> Integer
eval (Lit num) = num
eval (Add expr1 expr2) = eval expr1 + eval expr2

printExpr :: Expr -> String
printExpr expr = unwords $ strExpr expr
  where
    strExpr :: Expr -> [String]
    strExpr (Lit num) = [show num]
    strExpr (Add expr1 expr2) = strExpr expr1 ++ ["+"] ++ strExpr expr2

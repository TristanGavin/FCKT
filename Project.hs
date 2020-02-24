
data Expr
   = Get Var
   | Lit Int
   | Add Expr Expr
   | Mul Expr Expr
--   | Div Expr Expr
   | If Expr Expr Expr
  deriving (Eq,Show)

-- type String = [Char]

data Var
   = String 
  deriving (Eq, Show)
  
data Test
   = Equ Expr Expr
   | LTE Expr Expr
   | GTE Expr Expr
  deriving (Eq,Show)

data Stmt
   = Set Var Expr
   -- | While Test Stmt
   -- | Begin [Stmt]
  deriving (Eq,Show)


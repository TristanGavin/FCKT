
data Expr = Get Var
          | Lit Int
          | Add Expr Expr
          | Mul Expr Expr
--        | Div Expr Expr
          | If Expr Expr Expr
         deriving (Eq,Show)

-- type String = [Char]

data Type = TypeInt
          | TypeBool
--          | Float
--          | Char
--          | String
         deriving (Eq, Show) 

data Var  = Var [Char] Type Expr 
-- Sugar         | String Type Expr
         deriving (Eq, Show)
  
data Test = Equ Expr Expr
          | LTE Expr Expr
-- Sugar          | GTE Expr Expr
         deriving (Eq,Show)

data Stmt = Set Var Expr
   -- | While Test Stmt
   -- | Begin [Stmt]
         deriving (Eq,Show)


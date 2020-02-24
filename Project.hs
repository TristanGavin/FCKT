-- *Abstract Syntax

--  
--  expr := 'Get'                                  gets the variable value
--        | int                                    int literal
--        | expr '+' expr                          add two expressions
--        | expr '*' expr                          multiply two expression
--        | expr '/' expr                          divide two expressions.
--        | If expr 'then' expr 'else' expr        if else statmment 
--
--  var := string type                             defining a variable
--  
--  test := expr '=' expr                          check if two expr are equal                 
--        | expr '<' expr                          less than
--        | expr '>'                               greater than
--        | expr '<=' expr                         Less than or equal to
--        | expr '>=' expr                         Greater or equal to. 
--
--  data stmt := Set                               assigns value to a var
--


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
   | String Type Expr
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


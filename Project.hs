-- *Abstract Syntax

--
--  expr := 'Get'                                  gets the variable value
--        | int                                    int literal
--        | expr '+' expr                          add two expressions
--        | expr '*' expr                          multiply two expression
--        | expr '/' expr                          divide two expressions.
--        | If expr 'then' expr 'else' expr        if else statmment
--
--  type := int                                    integer value
--        | bool                                   boolean value
--        | float                                  floating point number
--        | char                                   ascii character
--        | string                                 array of chars
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


data Expr = Var String Type Expr
          -- | Get Var
          | Lit Int
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | If Expr Expr Expr
         deriving (Eq,Show)

-- type String = [Char]

-- Defining our basic data types
data Type = TypeInt
          | TypeBool
          | TypeFloat
          | TypeChar
          | TypeString
          | TypeError
         deriving (Eq, Show)

-- Defining a variable ie. from haskell to c code
-- Var x TypeInt Lit 5 => int x = 5;
data Var  = Var [Char] Type Expr
-- Sugar         | String Type Expr
--         deriving (Eq, Show)

data Test = Equ Expr Expr
          | LTE Expr Expr
-- Sugar          | GTE Expr Expr
         deriving (Eq,Show)

--data Stmt = Set Var Expr
   -- | While Test Stmt
   -- | Begin [Stmt]
--         deriving (Eq,Show)


expr :: Expr -> Int
expr (Var s varType expression)   = expr expression
expr (Lit i)                      = i
expr (Add left right)             = expr left + expr right
expr (Mult left right)            = expr left * expr right


test :: Expr
test = Var "Hello" Int (Lit 3)

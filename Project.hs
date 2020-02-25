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

-- Data type that
data TypeVal = IntI    Int
             | BoolB   Bool
             | FloatF  Float
             | CharC   Char
             | StringS String
             | TypeError
             deriving (Eq,Show)

data Expr    = Var [Char] TypeVal
             | Value TypeVal
             | Add Expr Expr
             | Mul Expr Expr
             | Div Expr Expr
             | If Expr Expr Expr
             | Equ Expr Expr
             | LTE Expr Expr
--             | TypeVal Val
            deriving (Eq,Show)

-- type Numb = Either Int Float
-- type String = [Char]

-- Defining a variable ie. from haskell to c code
-- Var x TypeInt Lit 5 => int x = 5;
-- data Var     = Var [Char] TypeVal
-- -- Sugar         | String Type Expr
--         deriving (Eq, Show)

-- data Test = Equ Expr Expr
--           | LTE Expr Expr
-- -- Sugar          | GTE Expr Expr
--          deriving (Eq,Show)

-- data Stmt = Set Var Expr
--    | While Test Stmt
--    | Begin [Stmt]
--         deriving (Eq,Show)

expr :: Expr -> TypeVal
expr (Var name tval)    = tval
expr (Value tval)       = tval
expr (Add left right)   = case (expr left, expr right) of
                        (IntI i, IntI j)     -> IntI (i+j)
                        (FloatF i, FloatF j) -> FloatF (i+j)
                        (CharC s, CharC t)   -> StringS [s, t]
                        (CharC s, StringS ts)   -> StringS (s:ts)
                        (StringS ss, CharC t)   -> StringS (ss++[t])
                        (StringS ss, StringS ts) -> StringS (ss ++ ts)
                        _                    -> TypeError
expr (Mul left right)   = case (expr left, expr right) of
                        (IntI i, IntI j)     -> IntI (i*j)
                        (FloatF i, FloatF j) -> FloatF (i*j)
                        _                    -> TypeError
expr (Div left right)   = case (expr left, expr right) of
                        (IntI i, IntI j)     -> IntI (i `div` j)
                        _ -> TypeError
expr (If c t e)         = case expr c of
                        BoolB True -> expr t
                        BoolB False -> expr e
                        _ -> TypeError
expr (Equ left right)   = case (expr left, expr right) of
                        (IntI i, IntI j) -> BoolB (i == j)
                        (BoolB a, BoolB b) -> BoolB (a == b)
                        _ -> TypeError


test :: Expr
test = Var "Test" (IntI 4)

hello :: Expr
hello = Value (StringS "Hello ")

world :: Expr
world = Value (StringS "World!")

char1 :: Expr
char1 = Value (CharC 'A')

char2 :: Expr
char2 = Value (CharC 'B')

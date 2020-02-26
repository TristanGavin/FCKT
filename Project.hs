-- *Abstract Syntax
module Fckt where

import Data.Map (Map,fromList,lookup,insert)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)

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
type Var = String
-- Data type that
-- data TypeVal = IntI    Int
--              | BoolB   Bool
--              | FloatF  Float
--              | CharC   Char
--              | StringS String
--              | TypeError
type Number = Either Int Float

data Type = TypeInt
          | TypeBool
          | TypeFloat
          -- | TypeChar
          | TypeString
          | TypeError

data Expr    = Get Var
             | Lit Number
             | Str String
             | Add Expr Expr
             | Mul Expr Expr
             | Div Expr Expr
             | Equ Expr Expr
             | LT  Expr Expr
             | Neg Expr
             -- | If Expr Expr Expr
            deriving (Eq,Show)

data Line    = Set Var Expr
             | If Expr Line Line
             | While Expr Line
             | Chunk [Line]
             deriving (Eq, Show)

type Decl    = (Var, Type) -- = (Var, Either Type Expr)

data Program = Prog [Decl] Line
             deriving (Eq, Show)

type Env a = Map Var a

typeExpr :: Expr -> Env Type -> Maybe Type
typeExpr (Lit x)  _ = 
--
-- -- Defining a variable ie. from haskell to c code
-- -- Var x TypeInt Lit 5 => int x = 5;
-- data Var     = Var [Char] Type Expr
-- -- Sugar         | String Type Expr
--          deriving (Eq, Show)
--
-- data Test = Equ Expr Expr
--           | LTE Expr Expr
-- -- Sugar          | GTE Expr Expr
--          deriving (Eq,Show)
--
-- data Stmt = Set Var Expr
--    -- | While Test Stmt
--    -- | Begin [Stmt]
--          deriving (Eq,Show)

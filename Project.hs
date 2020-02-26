-- *Abstract Syntax
module Fckt where

import Data.Map (Map,fromList,lookup,insert)
import Data.Maybe (fromJust)
import Prelude hiding (lookup, LT)

--  Number := Either (Int, Float)
--
--  expr := var                                    gets the variable value
--    	| Number                               int or float literal
--    	| expr '+' expr                        add two expressions. return number or string
--    	| expr '*' expr                        multiply two expression. return number
--    	| expr '/' expr                        divide two expressions. return number
--    	| expr '==' expr                       checks if two expressions are equal (int, float, bool, string) returns bool
--    	| 'not' expr 			         Not either turns a number to a negative, or negates a boolean
--    	| expr '<' expr                        checks if an expression is less than another expression returns bool
--          | str                                  String
--
--
--  line := 'set' var expr                         bind a variable to a expression
--       | if expr then line else line             the if statement evaluates an expression runs statement according to val of expression.
--       | 'while' expr 'do' line                  while expression is true do a statement
--       | [line]                                  this is a block statement: like { code } in C

--  type := 'int'                                  integer value
--          | 'bool'                               boolean value
--          | 'float'                              floating point number
--          | 'string'                             array of chars
--          | error
--
--  var := var string                          	defining a variable
--
--  decl := var : either type expr  				declaring a variable is a tuple of a variable name, and a type or an expr
--
--    prog := [decl] stmt   							a program is a declaration of variables followed by statments


type Var = String

type Number = Either Int Float

data Type = TypeInt
          | TypeBool
          | TypeFloat
          -- | TypeChar
          | TypeString
          | TypeError
          deriving (Eq, Show)

data Expr = Get Var
          | Lit Number
          | Str String
          | Add Expr Expr
          | Mul Expr Expr
          | DivI Expr Expr
          | DivF Expr Expr
          | Equ Expr Expr
          | LT  Expr Expr
          | Neg Expr
       -- | If Expr Expr Expr
    deriving (Eq,Show)

data Line = Set Var Expr
          | If Expr Line Line
          | While Expr Line
          | Chunk [Line]
    deriving (Eq, Show)

type Decl = (Var, Type) -- = (Var, Either Type Expr)

data Program = Prog [Decl] Line
        deriving (Eq, Show)

type Env a = Map Var a

typeExpr :: Expr -> Env Type -> Type
typeExpr (Get var)         env = case lookup var env of
                                              Just a  -> a
                                              Nothing -> TypeError
typeExpr (Lit num)           _ = case num of
                                    Left _  -> TypeInt
                                    Right _ -> TypeFloat
typeExpr (Str string)      env = TypeString
typeExpr (Add left right)  env = case (typeExpr left env, typeExpr right env) of
                                    (TypeBool, TypeBool)     -> TypeError
                                    (TypeFloat, TypeInt)     -> TypeFloat
                                    (TypeInt, TypeFloat)     -> TypeFloat
                                    (a, b)                   -> if a == b then a else TypeError
                                  --  (_, _)                   -> TypeError
typeExpr (Mul left right)  env = case (typeExpr left env, typeExpr right env) of
                                    (TypeBool, TypeBool)     -> TypeError
                                    (TypeString, TypeString) -> TypeError
                                    (TypeFloat, TypeInt)     -> TypeFloat
                                    (TypeInt, TypeFloat)     -> TypeFloat
                                    (a, b)                   -> if a == b then a else TypeError
                                  --  (_, _)                   -> TypeError
typeExpr (DivI left right) env = case (typeExpr left env, typeExpr right env) of
                                    (TypeBool, TypeBool)     -> TypeError
                                    (TypeString, TypeString) -> TypeError
                                    (TypeFloat, TypeInt)     -> TypeInt
                                    (TypeInt, TypeFloat)     -> TypeInt
                                    (a, b)                   -> if a == b then TypeInt else TypeError
                                  --  (_, _)                   -> TypeError
typeExpr (DivF left right) env = case (typeExpr left env, typeExpr right env) of
                                    (TypeBool, TypeBool)     -> TypeError
                                    (TypeString, TypeString) -> TypeError
                                    (TypeFloat, TypeInt)     -> TypeFloat
                                    (TypeInt, TypeFloat)     -> TypeFloat
                                    (a, b)                   -> if a == b then TypeFloat else TypeError
                                --    (_, _)                   -> TypeError
typeExpr (Equ left right)  env = case (typeExpr left env, typeExpr right env) of
                                    (a, b)                   -> if a == b then TypeBool else TypeError
--could add                         (TypeInt, TypeFloat) -> TypeBool
                                --    _                    -> TypeError
typeExpr (Neg expr)        env = typeExpr expr env   -- Negating a string returns reversed string
typeExpr (LT left right)   env = case (typeExpr left env, typeExpr right env) of
                                    (a, b) -> if a == b then TypeBool else TypeError --strings will be len(s)<len(s1)
--typeExpr (Str string)        _ = TypeString



typeLine :: Line -> Env Type -> Bool
typeLine (Set var expr)        env = case (lookup var env, typeExpr expr env) of
                                        (Just typevar, TypeError) -> False
                                        (Just typevar, typeexpr)  -> typevar == typeexpr
                                        _                         -> False
typeLine (If expr line1 line2) env = case typeExpr expr env of
                                        TypeBool -> typeLine line1 env && typeLine line2 env
                                        _        -> False
typeLine (While expr line)     env = case typeExpr expr env of
                                        TypeBool -> typeLine line env
                                        _        -> False
typeLine (Chunk lines)         env = all (\line -> typeLine line env) lines
--

typeProgram :: Program -> Bool
typeProgram (Prog declarations line) = typeLine line (fromList declarations)

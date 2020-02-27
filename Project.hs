-- *Abstract Syntax
module Fckt where

import Data.Map (Map,fromList,lookup,insert)
import Data.Maybe (fromJust)
import Prelude hiding (lookup, LT)

-- Abstract Syntax:
--
--  var := Var string                 Defining a variable name
--
--  number := Either (Int, Float)     Defining a number as potentially being Int or Float
--
--  expr := Var                       Gets the variable value
--        | Number                    Int or float literal
--        | expr '+' expr             Add two expressions. return number or string
--        | expr '*' expr             Multiply two expressions. return number
--        | expr '/' expr             Divide two expressions. return number
--        | expr '==' expr            Checks if two expressions are equal returns bool
--        | 'not' expr                Either turns a number to a negative, or negates a boolean
--        | expr '<' expr             Checks if an expression < another expression, returns bool
--        | str                       String
--
--  line := 'set' var expr                    Bind a variable to a expression
--        | if expr then line else line       Evaluates expression, runs statement accordingly
--        | 'while' expr 'do' line            While expression is true do a statement
--        | [line]                            This is a block statement: like { code } in C
--
--  type := 'int'                     Integer value
--        | 'bool'                    Boolean value
--        | 'float'                   Floating point number
--        | 'string'                  Array of chars
--        | 'error'                   Generic error
--
--  decl := var : Either (type, expr)    Declaring a variable as tuple (var name, type/expr)
--
--  prog := [decl] line               A program is a declaration of variables, then statements



type Var = String

type Number = Either Int Float

data Type = TypeInt
          | TypeBool
          | TypeFloat
--        | TypeChar
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

data Program = Prog [Decl] Line
    deriving (Eq, Show)

type Decl = (Var, Type) -- = (Var, Either Type Expr)

type Env a = Map Var a


-- Expressions:
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
typeExpr (Mul left right)  env = case (typeExpr left env, typeExpr right env) of
                                    (TypeBool, TypeBool)     -> TypeError
                                    (TypeString, TypeString) -> TypeError
                                    (TypeFloat, TypeInt)     -> TypeFloat
                                    (TypeInt, TypeFloat)     -> TypeFloat
                                    (a, b)                   -> if a == b then a else TypeError
typeExpr (DivI left right) env = case (typeExpr left env, typeExpr right env) of
                                    (TypeBool, TypeBool)     -> TypeError
                                    (TypeString, TypeString) -> TypeError
                                    (TypeFloat, TypeInt)     -> TypeInt
                                    (TypeInt, TypeFloat)     -> TypeInt
                                    (a, b)                   -> if a == b then TypeInt else TypeError
typeExpr (DivF left right) env = case (typeExpr left env, typeExpr right env) of
                                    (TypeBool, TypeBool)     -> TypeError
                                    (TypeString, TypeString) -> TypeError
                                    (TypeFloat, TypeInt)     -> TypeFloat
                                    (TypeInt, TypeFloat)     -> TypeFloat
                                    (a, b)                   -> if a == b then TypeFloat else TypeError
typeExpr (Equ left right)  env = case (typeExpr left env, typeExpr right env) of
                                    (a, b)                   -> if a == b then TypeBool else TypeError
--could add                         (TypeInt, TypeFloat) -> TypeBool
typeExpr (Neg expr)        env = typeExpr expr env   -- Negating a string returns reversed string
typeExpr (LT left right)   env = case (typeExpr left env, typeExpr right env) of
                                    (TypeString, TypeString) -> TypeError
                                    (TypeBool, TypeBool)     -> TypeError
                                    (a, b) -> if a == b then TypeBool else TypeError --strings will be len(s)<len(s1)


-- Statements:
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


-- type Val = Bool || String
-- -- Program:
typeProgram :: Program -> Bool
typeProgram (Prog declarations line) = typeLine line (fromList declarations)
--
-- -- Evaluation functions:
type Val = Either Number (Either String Bool)

evalExpr :: Expr -> Env Val -> Val
evalExpr (Get var) env = case lookup var env of
                         Just val -> val
                         Nothing -> error "internal error: undefined variable"
evalExpr (Lit num) _   = Left num
evalExpr (Str str) _   = Right (Left str)
evalExpr (Add left right) env = case (evalExpr left env, evalExpr right env) of
                               (Left (Right n),  Left (Left m)  )    -> Left (Right (n + fromIntegral(m)))  -- Float + Int
                               (Left (Left n),   Left (Right m) )    -> Left (Right (m + fromIntegral(n)))  -- Int + Float
                               (Left (Left n),   Left (Left m)  )    -> Left (Left  (n + m))                -- Int + Int
                               (Left (Right n),  Left (Right m) )    -> Left (Right (n + m))                -- Float + Float
                               (Right (Left sa), Right (Left sb))    -> Right (Left (sa ++ sb))             -- String + String
evalExpr (Mul left right) env  = case (evalExpr left env, evalExpr right env) of
                               (Left (Right n),  Left (Left m)  )    -> Left (Right (n * fromIntegral(m)))  -- Float * Int
                               (Left (Left n),   Left (Right m) )    -> Left (Right (m * fromIntegral(n)))  -- Int * Float
                               (Left (Left n),   Left (Left m)  )    -> Left (Left  (n * m))                -- Int * Int
                               (Left (Right n),  Left (Right m) )    -> Left (Right (n * m))                -- Float * Float
evalExpr (DivI left right) env = case (evalExpr left env, evalExpr right env) of
                               (Left (Right n),  Left (Left m)  )    -> Left (Left (floor (n / fromIntegral m)))  -- Float div Int
                               (Left (Left n),   Left (Right m) )    -> Left (Left (floor (fromIntegral n / m)))  -- Int div Float
                               (Left (Left n),   Left (Left m)  )    -> Left (Left (n `div` m))                   -- Int div Int
                               (Left (Right n),  Left (Right m) )    -> Left (Left (floor (n / m)))               -- Float div Float
evalExpr (DivF left right) env = case (evalExpr left env, evalExpr right env) of
                               (Left (Right n),  Left (Left m)  )    -> Left (Right (n / fromIntegral m))               -- Float / Int
                               (Left (Left n),   Left (Right m) )    -> Left (Right (fromIntegral n / m))               -- Int / Float
                               (Left (Left n),   Left (Left m)  )    -> Left (Right (fromIntegral n / fromIntegral m))  -- Int / Int
                               (Left (Right n),  Left (Right m) )    -> Left (Right (n / m))                            -- Float / Float
evalExpr (Equ left right) env  = case (evalExpr left env, evalExpr right env) of
                               (a, b)                                -> Right (Right (a == b))
evalExpr (Neg expr) env        = case evalExpr expr env of
                               Left (Left a)                         -> Left (Left (-a))
                               Left (Right a)                        -> Left (Right (-a))
                               Right (Left s)                        -> Right (Left (negString s))
                               Right (Right b)                       -> Right (Right (not b))
evalExpr (LT left right) env  = case (evalExpr left env, evalExpr right env) of
                              (Left (Right n),  Left (Left m)  )    -> Right (Right (n < fromIntegral m))              -- Float / Int
                              (Left (Left n),   Left (Right m) )    -> Right (Right (fromIntegral n < m))              -- Int / Float
                              (Left (Left n),   Left (Left m)  )    -> Right (Right (n < m))               -- Int / Int
                              (Left (Right n),  Left (Right m) )    -> Right (Right (n < m))

negString :: String -> String
negString [] = []
negString (s:ss) = (negString ss) ++ [s]

evalInt :: Expr -> Env Val -> Int
evalInt expr env = case evalExpr expr env of
  Left (Left n) -> n
  otherwise -> error "internal error: expected Int, received something else"

evalFloat :: Expr -> Env Val -> Float
evalFloat expr env = case evalExpr expr env of
  Left (Right n) -> n
  otherwise -> error "internal error: expected Float, received something else"

--evalNum (Add left right) m =

-- evalExpr :: Expr -> Env a -> a
-- evalExpr (Str a)  = num

-- evalExpr :: a -> Env a -> b
-- evalExpr a env = case typeExpr a env of
--   TypeInt -> a
-- test2 :: a -> b -> a
-- test2 a b = a
--
-- test :: (a, b) -> a
-- test (a, b) = a

-- Evaluation functions:
--evalExpr :: Expr -> Env Var -> Value
--evalExpr = Undefined

--evalLine :: Expr -> Env Var -> Value
--evalLine = Undefined

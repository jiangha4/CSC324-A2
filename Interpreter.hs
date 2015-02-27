{- Assignment 2 - A Racket Interpreter

This module is the main program for the interpreter.
All of your work should go into this file.

We have provided a skeleton interpreter which can be run
successfully on sample.rkt; it is your job to extend this
program to handle the full range of Paddle.

In the space below, please list your group member(s):
g3kooks :: Philip Kukulak
g3jiangh :: Haohan Jiang
-}

module Interpreter (main) where

import BaseParser (BaseExpr(LiteralInt, LiteralBool, Atom, Compound), parseFile)
import Data.List
import System.Environment (getArgs)


-- |Run interpreter on an input file,
--  either from commandline or user input.
--  You should not need to change this function.
main :: IO ()
main =
    getArgs >>= \args ->
    if length args > 0
    then
        parseFile (head args) >>= \baseTree ->
        putStr (interpretPaddle baseTree)
    else
        putStrLn "Enter the name of a file: " >>
        getLine >>= \file ->
        parseFile file >>= \baseTree ->
        putStr (interpretPaddle baseTree)


-- |Take the output of the base parser and interpret it,
--  first constructing the AST, then evaluating it,
--  and finally returning string representations of the results.
--  You will need to make this function more robust against errors.
interpretPaddle :: Maybe [BaseExpr] -> String
interpretPaddle (Just exprs) =
    let ast = map parseExpr exprs
        vals = map evaluate ast
    in
        -- String representations of each value, joined with newlines
        unlines (map show vals)


-- An expression data type
data Expr = Number Integer |
            Boolean Bool |
            If Expr Expr Expr |
            Add Expr Expr |
            Multiply Expr Expr |
            Eq Expr Expr

instance Show Expr where
    show (Number x) = show x
    show (Boolean True) = "#t"
    show (Boolean False) = "#f"
    -- Note: the following definition is not necessary for this assignment,
    -- but you may find it helpful to define string representations of all
    -- expression forms.
    show (If e1 e2 e3) =
        "(if " ++ show e1 ++ " " ++ show e2 ++ " " ++ show e3 ++ ")"
    show (Add e1 e2) =
        "(+ " ++ show e1 ++ show e2 ++ ")"
    show (Multiply e1 e2) =
        "(* " ++ show e1 ++ show e2 ++ ")"
    show (Eq e1 e2) =
        "(equal? " ++ show e1 ++ show e2 ++ ")"


-- |Take a base tree produced by the starter code,
--  and transform it into a proper AST.
parseExpr :: BaseExpr -> Expr
parseExpr (LiteralInt n) = Number n
parseExpr (LiteralBool b) = Boolean b
parseExpr (Compound [Atom "if", b, x, y]) =
    If (parseExpr b) (parseExpr x) (parseExpr y)
parseExpr (Compound [Atom "+", x, y]) =
    Add (parseExpr x) (parseExpr y)
parseExpr (Compound [Atom "*", x, y]) =
    Multiply (parseExpr x) (parseExpr y)
parseExpr (Compound [Atom "equal?", x, y]) =
    Eq (parseExpr x) (parseExpr y)

-- |Evaluate an AST by simplifying it into
--  a number, boolean, list, or function value.
evaluate :: Expr -> Expr
evaluate (Number n) = Number n
evaluate (Boolean b) = Boolean b
evaluate (If cond x y) =
    case cond of
        Boolean True -> x
        Boolean False -> y

-- |Evaluate addition.
evaluate (Add (Number x) (Number y)) =
    Number (x + y)
evaluate (Add x y) =
    (evaluate (Add (evaluate x) (evaluate y)))

-- |Evaluate multiplication.
evaluate (Multiply (Number x) (Number y)) =
    Number (x * y)
evaluate (Multiply x y) =
    (evaluate (Multiply (evaluate x) (evaluate y)))

-- |Evaluate equality.
evaluate (Eq (Number x) (Number y)) =
   Boolean (x == y)
evaluate (Eq (Boolean x) (Boolean y)) =
   Boolean (x == y)
evaluate (Eq x y) =
   (evaluate (Eq (evaluate x) (evaluate y)))

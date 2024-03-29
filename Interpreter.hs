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

{- TODO : 
    - No errors for List functions
    - Cond
    - Defining functions
    - Bindings and closures
    - Definitations
    - Part 2
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

-- BaseParse -> parseExpr to construct AST -> evaulate
-- An expression data type: This can be 
   -- 1. Literal or identifier
   -- 2. Function application or syntax
   -- 3. Function creation expression
-- This defines a new data type 'Expr' and The RHS are the constructors for
-- an 'Expr'
-- An expression data type
data Expr = Number Integer |
            Boolean Bool |
            If Expr Expr Expr |
            Add Expr Expr |
            Multiply Expr Expr |
            Eq Expr Expr |
            Lt Expr Expr |
            Not Expr |
            And Expr Expr |
            Or Expr Expr |
            Cond [(Expr, Expr)] |
            Else (Expr, Expr) |
            List [Expr] |
            Empty Expr |
            First Expr |
            Rest Expr

data Identifier = ID [Char]

data SymbolTable = Bindings [(Identifier, Expr)]

-- |Removes square brackets and inner-apostrophes
--  from the string representation of the list.
fixListShow :: [Char] -> [Char] -> [Char]
fixListShow = filter . flip notElem

-- |Replaces commas with spaces.
spaceList :: [Char] -> [Char]
spaceList [] = []
spaceList str =
    let 
        s = head str
        t = tail str
    in
        if s == ','
        then ' ' : spaceList t
        else s : spaceList t

instance Show Expr where
    show (Number x) = show x
    show (Boolean True) = "#t"
    show (Boolean False) = "#f"
    show (List e) = 
        "'" ++  fixListShow "[]'" ("(" ++ spaceList (show e) ++ ")")

-- |Take a base tree produced by the starter code,
--  and transform it into a proper AST.
parseExpr :: BaseExpr -> Expr
parseExpr (LiteralInt n) = Number n
parseExpr (LiteralBool b) = Boolean b

-- |Parse 'if' expressions.
parseExpr (Compound [Atom "if", b, x, y]) =
    If (parseExpr b) (parseExpr x) (parseExpr y)

-- |Parse addition.
parseExpr (Compound [Atom "+", x, y]) =
    Add (parseExpr x) (parseExpr y)

-- |Parse multiplication.
parseExpr (Compound [Atom "*", x, y]) =
    Multiply (parseExpr x) (parseExpr y)

-- |Parse 'equal?'.
parseExpr (Compound [Atom "equal?", x, y]) =
    Eq (parseExpr x) (parseExpr y)

-- |Parse '<'.
parseExpr (Compound [Atom "<", x, y]) =
    Lt (parseExpr x) (parseExpr y)

-- |Parse negation.
parseExpr (Compound [Atom "not", x]) =
    Not (parseExpr x)

-- |Parse 'and'.
parseExpr (Compound [Atom "and", x, y]) =
    And (parseExpr x) (parseExpr y)

-- |Parse 'or'.
parseExpr (Compound [Atom "or", x, y]) =
    Or (parseExpr x) (parseExpr y)

-- |Parse list construction.
parseExpr (Compound (Atom "list" : val)) = 
    List (map parseExpr val)

-- |Parse 'first' for lists.
parseExpr (Compound [Atom "first", lst]) =
    First (parseExpr lst)

-- |Parse 'rest' for lists.
parseExpr (Compound [Atom "rest", lst]) =
    Rest (parseExpr lst)

-- |Parse 'empty?' for lists.
parseExpr (Compound [Atom "empty?", lst]) =
    Empty (parseExpr lst)

-- |Parse 'cond'.
parseExpr (Compound (Atom "cond" : val)) =
    let
        condEntry (Compound [Atom "else", x]) = (Boolean True, parseExpr x)
        condEntry (Compound [x, y]) = (parseExpr x, parseExpr y)
        parseCond = map condEntry
    in
        Cond (parseCond val)


-- |Evaluate an AST by simplifying it into
--  a number, boolean, list, or function value.
evaluate :: Expr -> Expr
evaluate (Number n) = Number n
evaluate (Boolean b) = Boolean b
evaluate (List l) = List (map evaluate l)

-- |Evaluate if-then-else.
evaluate (If cond x y) =
    case (evaluate cond) of
        Boolean True -> (evaluate x)
        Boolean False -> (evaluate y)

-- |Evaluate addition.
evaluate (Add (Number x) (Number y)) =
    Number (x + y)
evaluate (Add x y) =
    evaluate (Add (evaluate x) (evaluate y))

-- |Evaluate multiplication.
evaluate (Multiply (Number x) (Number y)) =
    Number (x * y)
evaluate (Multiply x y) =
    evaluate (Multiply (evaluate x) (evaluate y))

-- |Evaluate equality.

-- |Evaluate list equality. Lists are a special case,
--  since they are not atomic values.
evaluate (Eq (List l1) (List l2)) =
    let 
        zippedList = zip l1 l2
        equalPairs = map (\(x, y) -> (evaluate (Eq x y)))
        allTrue = foldl (\x y -> (evaluate (And x y))) (Boolean True)
    in  
        if (length l1) /= (length l2)
        then Boolean False
        else allTrue (equalPairs zippedList)

evaluate (Eq (Number x) (Number y)) =
    Boolean (x == y)
evaluate (Eq (Boolean x) (Boolean y)) =
    Boolean (x == y)
evaluate (Eq x y) =
    evaluate (Eq (evaluate x) (evaluate y))

-- |Evaluate less-than comparison.
evaluate (Lt (Number x) (Number y)) =
    Boolean (x < y)
evaluate (Lt x y) =
    evaluate (Lt (evaluate x) (evaluate y))

-- |Evaluate logical negation.
evaluate (Not (Boolean x)) =
    Boolean (not x)
evaluate (Not x) =
    evaluate (Not (evaluate x))

-- |Evaluate logical and.
evaluate (And (Boolean x) (Boolean y)) =
    Boolean (x && y)
evaluate (And x y) =
    evaluate (And (evaluate x) (evaluate y))

-- |Evaluate logical or.
evaluate (Or (Boolean x) (Boolean y)) =
    Boolean (x || y)
evaluate (Or x y) =
    evaluate (Or (evaluate x) (evaluate y))

-- |Evaluate 'empty?' for lists.
evaluate (Empty (List lst)) =
    Boolean (null lst)

-- |Evaluate 'first' for lists.
evaluate (First (List lst)) =
    head lst
evaluate (First lst) =
    First (evaluate lst)

-- |Evaluate 'rest' for lists.
evaluate (Rest (List lst)) =
    List (tail lst)
evaluate (Rest lst) =
    Rest (evaluate lst)

-- |Evaluate 'cond'.
evaluate (Cond ((Boolean True, e) : _)) =
    evaluate e
evaluate (Cond ((Boolean False, _) : lst)) =
    (evaluate (Cond lst))
evaluate (Cond lst) =
    let
        evalCond = map (\(x, y) -> (evaluate x, evaluate y))
    in
        evaluate (Cond (evalCond lst))


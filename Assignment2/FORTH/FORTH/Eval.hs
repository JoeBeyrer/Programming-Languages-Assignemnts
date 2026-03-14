module Eval where
-- This file contains definitions for functions and operators

import Val

import Data.Char

-- main evaluation function for operators and 
-- built-in FORTH functions with no output
-- takes a string and a stack and returns the stack
-- resulting from evaluation of the function
eval :: String -> [Val] -> [Val]
-- Multiplication
-- if arguments are integers, keep result as integer
eval "*" (Integer x: Integer y:tl) = Integer (y*x) : tl
-- if any argument is float, make result a float
eval "*" (x:y:tl) = (Real $ toFloat y * toFloat x) : tl 
-- any remaining cases are stacks too short
eval "*" _ = error("Stack underflow")

-- Division
-- if arguments are integers, keep result as integer
eval "/" (Integer x: Integer y:tl) = Integer (y `div` x) : tl
-- if any argument is float, make result a float
eval "/" (x:y:tl) = (Real $ toFloat y / toFloat x) : tl 
-- any remaining cases are stacks too short
eval "/" _ = error("Stack underflow")

-- Subtraction
-- if arguments are integers, keep result as integer
eval "-" (Integer x: Integer y:tl) = Integer (y-x) : tl
-- if any argument is float, make result a float
eval "-" (x:y:tl) = (Real $ toFloat y - toFloat x) : tl 
-- any remaining cases are stacks too short
eval "-" _ = error("Stack underflow")

-- Addition
-- if arguments are integers, keep result as integer
eval "+" (Integer x: Integer y:tl) = Integer (y+x) : tl
-- if any argument is float, make result a float
eval "+" (x:y:tl) = (Real $ toFloat y + toFloat x) : tl 
-- any remaining cases are stacks too short
eval "+" _ = error("Stack underflow")

-- Power
-- if arguments are integers, keep result as integer
eval "^" (Integer x: Integer y:tl) = Integer (y^x) : tl
-- if any argument is float, make result a float
eval "^" (x:y:tl) = (Real $ toFloat y ** toFloat x) : tl 
-- any remaining cases are stacks too short
eval "^" _ = error("Stack underflow")


-- Duplicate the element at the top of the stack
eval "DUP" (x:tl) = (x:x:tl)
eval "DUP" [] = error("Stack underflow")


-- Takes a number from the stack and prints the character with the corresponding ASCII code
eval "EMIT" (Integer x:tl) = Id [chr x]: tl
eval "EMIT" [] = error("Stack underflow")

-- Prints a new line (for nice formating)
eval "CR" tl = Id "\n" : tl

-- converts the argument into a string (needs to work for all types)
eval "STR" (Integer x : tl) = Id (show x) : tl
eval "STR" (Real x : tl) = Id (show x) : tl
eval "STR" (Id x : tl) = Id x : tl
eval "STR" [] = error("Stack underflow")

-- concatenates 2  strings from the stack (errors if arguments not strings)
eval "CONCAT2" (Id x: Id y:tl) = Id (y ++ x) : tl
eval "CONCAT2" [] = error("Stack underflow")
eval "CONCAT2" [_] = error("Stack underflow")

-- concatenates 3 strings from the stack (errors if arguments not strings)
eval "CONCAT3" (Id x: Id y: Id z: tl) = Id (z ++ y ++ x) : tl
eval "CONCAT3" [] = error("Stack underflow")
eval "CONCAT3" [_] = error("Stack underflow")
eval "CONCAT3" [_, _] = error("Stack underflow")


-- this must be the last rule
-- it assumes that no match is made and preserves the string as argument
eval s l = Id s : l 


-- variant of eval with output
-- state is a stack and string pair
evalOut :: String -> ([Val], String) -> ([Val], String) 
-- print element at the top of the stack
evalOut "." (Id x:tl, out) = (tl, out ++ x)
evalOut "." (Integer i:tl, out) = (tl, out ++ (show i))
evalOut "." (Real x:tl, out) = (tl, out ++ (show x))
evalOut "." ([], _) = error "Stack underflow"

-- this has to be the last case
-- if no special case, ask eval to deal with it and propagate output
evalOut op (stack, out) = (eval op stack, out)
{-|
Module      : HaskellExercises02.Exercises02
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Haskell exercise template Set 02 - McMaster CS 1JC3 2021
-}
module Exercises02 where

import Prelude hiding ((||),(&&),abs)

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------
macid = "gupta67"

-- NOTE see the wikipedia page on truth tables https://en.wikipedia.org/wiki/Truth_table
--      (particularly the sections Logical conjuction, Logical disjunction, etc) for a reference
--      on the different boolean operators

-- Exercise A
-----------------------------------------------------------------------------------------------------------
-- Implement Logical disjunction (OR) using pattern matching
-- NOTE feel free to change the function declaration (pattern match on the arguments), just don't change
--      the type decleration
-----------------------------------------------------------------------------------------------------------
(||) :: Bool -> Bool -> Bool
False || False = False 
x || y = True

-- Alternatively:
-- (||) :: Bool -> Bool -> Bool
-- x || y = case (x, y) of
--           (False, False)  -> False
--           _               -> True

-- Exercise B
-----------------------------------------------------------------------------------------------------------
-- Implement Logical conjunction (AND) using pattern matching
-----------------------------------------------------------------------------------------------------------
(&&) :: Bool -> Bool -> Bool
True && True = True
x && y = False

-- Exercise C
-----------------------------------------------------------------------------------------------------------
-- Implement Logical implication using pattern matching
-----------------------------------------------------------------------------------------------------------
(==>) :: Bool -> Bool -> Bool
True ==> False = False
x ==> y = True

-- Exercise D
-----------------------------------------------------------------------------------------------------------
-- Implement the function abs that returns the absolute value of a number
-----------------------------------------------------------------------------------------------------------
abs :: (Num a,Ord a) => a -> a
abs x 
  | x < 0       = -x
  | otherwise   = x

-- Exercise E
-----------------------------------------------------------------------------------------------------------
-- Implement a function that compares two floating point numbers, and returns True if they are within
-- a tolerance of 1e-4 of eachother
-- NOTE use the abs fine you just defined
-- NOTE^2 in general, you should use an operator like this instead of == on two floating point numbers
--        HOWEVER, you'll need to adjust the tolerance to suit different contexts
-----------------------------------------------------------------------------------------------------------
(=.) :: (Floating a,Ord a) => a -> a -> Bool
x =. y = abs(x - y) <= 1e-4 

-- Exercise F
-----------------------------------------------------------------------------------------------------------
-- Implement a function stack that takes the first element of a list and moves it to the back
-----------------------------------------------------------------------------------------------------------
stack :: [a] -> [a]
stack [] = []
stack (x:xs) = xs ++ [x] -- : operator to separate the head from the tail

-- Exercise G
-----------------------------------------------------------------------------------------------------------
-- Implement a function halves that takes a list of integers and divides each element of the list in two
-- (using the integer division operator `div`)
-- NOTE use the map function combined with a lambda expression to do the division
-----------------------------------------------------------------------------------------------------------
halves :: Integral a => [a] -> [a]
-- halves xs = map (\x -> div x 2) xs
-- halves xs = [ div x 2 | x <- xs ]

halves [] = []
halves (x:xs) = [div x 2] ++ halves xs -- recursive definition

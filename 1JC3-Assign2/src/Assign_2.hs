--
-- Module      : 1JC3-Assign2.Assign_2.hs
-- Copyright   :  (c) William M. Farmer 2024
-- License     :  GPL (see the LICENSE file)
-- Maintainer  :  none
-- Stability   :  experimental
-- Portability :  portable
--
-- Description:
--   Assignment 2 - McMaster CS 1JC3 2024
module Assign_2 where

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

-- Name: Ameya Gupta
-- Date: 18/10/2024
macid :: String
macid = "gupta67"

type GaussianInt = (Integer, Integer)

{- -----------------------------------------------------------------
 - gaussReal
 - -----------------------------------------------------------------
 - Description:
 -   returns the real component of a given GaussianInt
 -}
gaussReal :: GaussianInt -> Integer
gaussReal (a, _) = a

{- -----------------------------------------------------------------
 - gaussImag
 - -----------------------------------------------------------------
 - Description:
 -   returns the imaginary component of a given GaussianInt
 -}
gaussImag :: GaussianInt -> Integer
gaussImag (_, b) = b

{- -----------------------------------------------------------------
 - gaussConj
 - -----------------------------------------------------------------
 - Description:
 -   returns the conjugate of a given GaussianInt by multiplying the imaginary component by -1
 -}
gaussConj :: GaussianInt -> GaussianInt
gaussConj (a, b) = (a, -b)

{- -----------------------------------------------------------------
 - gaussAdd
 - -----------------------------------------------------------------
 - Description:
 -   adds the real and imaginary components of two GaussianInt's separately to form a GaussianInt addition
 -}
gaussAdd :: GaussianInt -> GaussianInt -> GaussianInt
gaussAdd (a0, b0) (a1, b1) = (a0 + a1, b0 + b1)

{- -----------------------------------------------------------------
 - gaussMul
 - -----------------------------------------------------------------
 - Description:
 -   multiplies two GaussianInt's together
 -}
gaussMul :: GaussianInt -> GaussianInt -> GaussianInt
gaussMul (a0, b0) (a1, b1) = (real, imag)
  where
    real = (a0 * a1) - (b0 * b1)
    imag = (a0 * b1) + (a1 * b0)

{- -----------------------------------------------------------------
 - gaussNorm
 - -----------------------------------------------------------------
 - Description:
 -   returns the norm of a GaussianInt
 -}
gaussNorm :: GaussianInt -> Integer
gaussNorm (a, b) = a ^ 2 + b ^ 2

{- -----------------------------------------------------------------
 - gaussAddList
 - -----------------------------------------------------------------
 - Description:
 -   recursively adds all GaussianInt's in a list
 -}
gaussAddList :: [GaussianInt] -> GaussianInt
gaussAddList [] = (0, 0)
gaussAddList (x : xs) = x `gaussAdd` gaussAddList xs

{- -----------------------------------------------------------------
 - gaussMulList
 - -----------------------------------------------------------------
 - Description:
 -   recursively multiplies all GaussianInt's in a list
 -}
gaussMulList :: [GaussianInt] -> GaussianInt
gaussMulList [] = (1, 0)
gaussMulList (x : xs) = x `gaussMul` gaussMulList xs

{- ------------------------------------------------------------------------
 - gaussCircle
 - ------------------------------------------------------------------------
 - Description:
 -   takes in some list of GaussianInt's as input, and returns a filtered list
 -   such that all GaussianInt's in the list have a norm less than some integer, n
 -}
gaussCircle :: [GaussianInt] -> Integer -> [GaussianInt]
gaussCircle vs n
  | n > 0 = [gInt | gInt <- vs, gaussNorm gInt < n]
  | otherwise = error "Norm bound must be nonnegative and not 0"

gold :: Float -> Float
gold x
  | x < 0.0 = gold (x / 3)
  | x == 0.0 = 1.0
  | x > 0.0 = gold (x / 3)

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [c] = True
isPalindrome (c : cs) =
  let ds = reverse cs
   in if c == head ds
        then isPalindrome (tail ds)
        else False

{-

 ********** TEST PLAN **********

--- gaussConj ---

Function: gaussConj
Test Case Number: 1
Input: (3, 4)
Expected Output: (3, -4)
Actual Output: (3, -4)

Function: gaussConj
Test Case Number: 2
Input: (-5, 2)
Expected Output: (-5, -2)
Actual Output: (-5, -2)

Function: gaussConj
Test Case Number: 3
Input: (0, -6)
Expected Output: (0, 6)
Actual Output: (0, 6)

--- gaussAdd ---

Function: gaussAdd
Test Case Number: 1
Input: (2, 3), (1, -4)
Expected Output: (3, -1)
Actual Output: (3, -1)

Function: gaussAdd
Test Case Number: 2
Input: (0, 0), (-3, 5)
Expected Output: (-3, 5)
Actual Output: (-3, 5)

Function: gaussAdd
Test Case Number: 3
Input: (-1, -1), (1, 1)
Expected Output: (0, 0)
Actual Output: (0, 0)

--- gaussMul ---

Function: gaussMul
Test Case Number: 1
Input: (2, 3), (1, -1)
Expected Output: (5, 1)
Actual Output: (5, 1)

Function: gaussMul
Test Case Number: 2
Input: (0, 1), (1, 1)
Expected Output: (-1, 1)
Actual Output: (-1, 1)

Function: gaussMul
Test Case Number: 3
Input: (-2, -2), (3, 4)
Expected Output: (2, -14)
Actual Output: (2, -14)

--- gaussNorm ---

Function: gaussNorm
Test Case Number: 1
Input: (3, 4)
Expected Output: 25
Actual Output: 25

Function: gaussNorm
Test Case Number: 2
Input: (1, -1)
Expected Output: 2
Actual Output: 2

Function: gaussNorm
Test Case Number: 3
Input: (-5, -12)
Expected Output: 169
Actual Output: 169

--- gaussAddList ---

Function: gaussAddList
Test Case Number: 1
Input: [(1, 1), (2, 3), (0, -4)]
Expected Output: (3, 0)
Actual Output: (3, 0)

Function: gaussAddList
Test Case Number: 2
Input: []
Expected Output: (0, 0)
Actual Output: (0, 0)

Function: gaussAddList
Test Case Number: 3
Input: [(5, 6)]
Expected Output: (5, 6)
Actual Output: (5, 6)

--- gaussMulList ---

Function: gaussMulList
Test Case Number: 1
Input: [(1, 1), (2, 3), (0, 1)]
Expected Output: (-3, 5)
Actual Output: (-3, 5)

Function: gaussMulList
Test Case Number: 2
Input: []
Expected Output: (1, 0)
Actual Output: (1, 0)

Function: gaussMulList
Test Case Number: 3
Input: [(2, 0), (3, 4)]
Expected Output: (6, 8)
Actual Output: (6, 8)

--- gaussCircle ---

Function: gaussCircle
Test Case Number: 1
Input: [(3, 4), (1, 1), (0, 2)] 30
Expected Output: [(3, 4), (1, 1), (0, 2)]
Actual Output: [(3, 4), (1, 1), (0, 2)]

Function: gaussCircle
Test Case Number: 2
Input: [(5, 6), (-1, -1), (2, 3)] -50
Expected Output: *** Exception: Norm bound must be nonnegative and not 0
Actual Output: *** Exception: Norm bound must be nonnegative and not 0

Function: gaussCircle
Test Case Number: 3
Input: [(3, 4), (0, 1), (7, 8)] 20
Expected Output: [(0, 1)]
Actual Output: [(0, 1)]

-}
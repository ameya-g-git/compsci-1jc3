-- |
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
-- 2) DO NOT REMOVE / ALTER TYPE DECLARATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLARATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: TODO add name
-- Date: TODO add date
macid :: String
macid = "SOLUTION"

type GaussianInt = (Integer, Integer)

{- -----------------------------------------------------------------
 - gaussReal
 - -----------------------------------------------------------------
 - Description:
 -   Computes the real part of a Gaussian integer.
 -}
gaussReal :: GaussianInt -> Integer
gaussReal (x, y) = x

{- -----------------------------------------------------------------
 - gaussImag
 - -----------------------------------------------------------------
 - Description:
 -   Computes the imaginary part of a Gaussian integer.
 -}
gaussImag :: GaussianInt -> Integer
gaussImag (x, y) = y

{- -----------------------------------------------------------------
 - gaussConj
 - -----------------------------------------------------------------
 - Description:
 -   Computes the conjugate of a Gaussian integer.
 -}
gaussConj :: GaussianInt -> GaussianInt
gaussConj (x, y) = (x, -y)

{- -----------------------------------------------------------------
 - gaussAdd
 - -----------------------------------------------------------------
 - Description:
 -   Adds two Gaussian integers.
 -}
gaussAdd :: GaussianInt -> GaussianInt -> GaussianInt
gaussAdd (x0, y0) (x1, y1) = ((x0 + x1), (y0 + y1))

{- -----------------------------------------------------------------
 - gaussMul
 - -----------------------------------------------------------------
 - Description:
 -   Multiplies two Gaussian integers.
 -}
gaussMul :: GaussianInt -> GaussianInt -> GaussianInt
gaussMul (x0, y0) (x1, y1) =
  let x' = x0 * x1 - y0 * y1
      y' = x0 * y1 + y0 * x1
   in (x', y')

{- -----------------------------------------------------------------
 - gaussNorm
 - -----------------------------------------------------------------
 - Description:
 -   Computes the norm of a Gaussian integer.
 -}
gaussNorm :: GaussianInt -> Integer
gaussNorm g = gaussReal (gaussMul g (gaussConj g))

{- -----------------------------------------------------------------
 - gaussAddList
 - -----------------------------------------------------------------
 - Description:
 -   Adds the members in a list of Gaussian integers.
 -}
gaussAddList :: [GaussianInt] -> GaussianInt
gaussAddList xs =
  case xs of
    [] -> (0, 0)
    y : ys -> gaussAdd y (gaussAddList ys)

{- -----------------------------------------------------------------
 - gaussMulList
 - -----------------------------------------------------------------
 - Description:
 -   Multiplies the members in a list of Gaussian integers.
 -}
gaussMulList :: [GaussianInt] -> GaussianInt
gaussMulList xs =
  case xs of
    [] -> (1, 0)
    y : ys -> gaussMul y (gaussAddList ys)

{- ------------------------------------------------------------------------
 - gaussCircle
 - ------------------------------------------------------------------------
 - Description:
 -   Given a list xs of Gaussian integers and a natural number n, computes
 -   the sublist of xs of Gaussian integers that have a norm < n.
 -}
gaussCircle :: [GaussianInt] -> Integer -> [GaussianInt]
gaussCircle xs n
  | n >= 0 = case xs of
      [] -> []
      y : ys ->
        if (gaussNorm y) < n
          then [y] ++ (gaussCircle ys n)
          else gaussCircle ys n
  | otherwise = []
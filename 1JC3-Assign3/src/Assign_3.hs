{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- |
-- Module      : 1JC3-Assign3.Assign_3.hs
-- Copyright   :  (c) William M. Farmer 2024
-- License     :  GPL (see the LICENSE file)
-- Maintainer  :  none
-- Stability   :  experimental
-- Portability :  portable
--
-- Description:
--   Assignment 3 - McMaster CS 1JC3 2024
module Assign_3 where

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E., THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E., RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (E.G., IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: Ameya Gupta
-- Date: 11/11/2024
macid :: String
macid = "gupta67"

{- -----------------------------------------------------------------
 - Datatypes
 - -----------------------------------------------------------------
 -}

data Poly a
  = X
  | Coef a
  | Sum (Poly a) (Poly a)
  | Prod (Poly a) (Poly a)
  deriving (Show)

newtype PolyList a = PolyList [a]
  deriving (Show)

{- -----------------------------------------------------------------
 - polyFun
 - -----------------------------------------------------------------
 - Description:
 -    Evalulates a Poly at some value c
 -}
polyFun :: (Num a) => Poly a -> a -> a
polyFun p c = case p of
  X -> c
  Coef a -> a
  Sum p1 p2 -> polyFun p1 c + polyFun p2 c
  Prod p1 p2 -> polyFun p1 c * polyFun p2 c

{- -----------------------------------------------------------------
 - polyDegree
 - -----------------------------------------------------------------
 - Description:
 -    Evaluates the degree of a given Poly, using the helper function:
 -    - isZeroPoly, which, when given a Poly of degree n (unknowing of if the polynomial is a zero polynomial or not),
 -      will evaluate the Poly from [1..n+1] to determine how many roots there are.
 -      If there are more roots than the presupposed degree of the Poly, then it must be a zero polynomial and thus the degree is undefined
 -}

{-
- any polynomial of degree n will have at most n real roots.
- isZeroPoly checks if a function is a zero polynomial by evaluating a Poly using values from [1..n+1],
- and checking to see if they all evaluate to 0. if they do, the function returns True
-}
isZeroPoly :: (Num a, Eq a) => Poly a -> Int -> Bool
isZeroPoly p deg = all (== 0) [polyFun p (fromIntegral x) | x <- [1 .. deg + 1]]
isZeroPoly _ 0 = True

polyDegree :: (Num a, Eq a) => Poly a -> Int
polyDegree (Coef x) = if x /= 0 then 0 else error "Zero polynomial is undefined"
polyDegree p = if isZeroPoly p degree then error "Zero polynomial is undefined" else degree
  where
    degree = case p of
      Prod (Coef 0) _ -> 0
      Prod _ (Coef 0) -> 0
      Coef _ -> 0
      X -> 1
      Sum p q -> max (polyDegree p) (polyDegree q)
      Prod p q -> polyDegree p + polyDegree q

{- -----------------------------------------------------------------
 - polyListFun
 - -----------------------------------------------------------------
 - Description:
 -    Evaluates a polynomial list representation (PolyList) at some value c
 -}
polyListFun :: (Num a) => PolyList a -> a -> a
polyListFun (PolyList (p : pl)) c = p + c * polyListFun (PolyList pl) c

{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description:
 -    Returns the degree of a PolyList by using the length of the properized PolyList and subtracting 1
 -}

-- Returns the actual list of coefficients from a PolyList
fromPolyList :: PolyList a -> [a]
fromPolyList (PolyList p) = p

-- Turns a (possibly) improper PolyList into a proper one by omitting 0s at the end of the list
properPolyList :: (Num a, Eq a) => PolyList a -> PolyList a
properPolyList (PolyList []) = PolyList []
properPolyList (PolyList pl) = if last pl == 0 then properPolyList (PolyList (init pl)) else PolyList pl

polyListDegree :: (Num a, Eq a) => PolyList a -> Int
polyListDegree (PolyList []) = error "Zero polynomial is undefined"
polyListDegree pl = if null properPl then error "Zero polynomial is undefined" else length properPl - 1
  where
    properPl = fromPolyList (properPolyList pl)

{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - Description:
 -    Adds the respective coefficients represented in two PolyLists to each other
 -    Uses concatenation helper function created for ease of implementation
 -}

concatPolyList :: PolyList a -> PolyList a -> PolyList a
concatPolyList (PolyList p) (PolyList q) = PolyList (p ++ q)

polyListSum :: (Num a) => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList (p : pl)) (PolyList (q : ql)) = PolyList [p + q] `concatPolyList` polyListSum (PolyList pl) (PolyList ql)
polyListSum pl ql = pl `concatPolyList` ql

{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description:
 -    Multiplies two polynomials represented by PolyLists using the helper functions:
 -    - polyListTermProd, which multiplies a PolyList by a monomial given its coefficient and degree,
 -    - polyListProdAux, which is an auxiliary function used to track the degree of the first element of the first polynomial
 -}

polyListProd :: (Num a) => PolyList a -> PolyList a -> PolyList a
polyListProd pl ql = polyListProdAux pl ql 0
  where
    -- polyListTermProd multiplies the PolyList inputted by the coefficient, and pads the start of the list with 0s depending on the degree of the term
    polyListTermProd :: (Num a, Num b, Enum b) => a -> b -> PolyList a -> PolyList a
    polyListTermProd coef deg (PolyList pl) = PolyList ([0 | _ <- [1 .. deg]] ++ map (coef *) pl)

    -- auxiliary function logic
    polyListProdAux :: (Num a, Num b, Enum b) => PolyList a -> PolyList a -> b -> PolyList a
    polyListProdAux (PolyList (p : pl)) ql n = polyListTermProd p n ql `polyListSum` polyListProdAux (PolyList pl) ql (n + 1)
    polyListProdAux (PolyList p) (PolyList q) _ = PolyList ([0 | _ <- [1 .. (length q)]])

{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description:
 -    Converts a PolyList representation of a polynomial to that of the Poly type, using the helper functions:
 -    - xExp, which raises X (Poly) to some inputted power
 -    - polyListToPolyAux, which is an auxiliary function used to track the degree of each consituent monomial of the Poly
 -}

polyListToPoly :: (Num a) => PolyList a -> Poly a
polyListToPoly pl = polyListToPolyAux pl 0
  where
    -- xExp takes an exponent value >= 0, and raises X to that power using repeated multiplication
    xExp :: (Num a, Ord a) => a -> Poly b
    xExp exp = if exp > 1 then Prod X (xExp (exp - 1)) else X

    -- auxiliary function to track the degree of each constituent monomial, create them, and Sum them together
    polyListToPolyAux :: (Num a, Num b, Ord b) => PolyList a -> b -> Poly a
    polyListToPolyAux (PolyList (p : pl)) deg
      | deg == 0 && null pl = Coef p -- PolyList has one term
      | deg == 0 && not (null pl) = Sum (Coef p) (polyListToPolyAux (PolyList pl) (deg + 1)) -- degree of 0 and more than one term means we add the coefficient to the polynomial
      | null pl = Prod (Coef p) (xExp deg) -- reached the last term in the PolyList (that has more than 1 term)
      | otherwise = Sum (Prod (Coef p) (xExp deg)) (polyListToPolyAux (PolyList pl) (deg + 1)) -- a degree > 0 is the product of the coefficient from the PolyList and X raised to that power, using the helper function xExp

{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
polyToPolyList :: (Num a) => Poly a -> PolyList a
polyToPolyList p = pl
  where
    pl = case p of
      Coef x -> PolyList [x]
      X -> PolyList [0, 1]
      Sum p q -> polyToPolyList p `polyListSum` polyToPolyList q
      Prod p q -> polyToPolyList p `polyListProd` polyToPolyList q

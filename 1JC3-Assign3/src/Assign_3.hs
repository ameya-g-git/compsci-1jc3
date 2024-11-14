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
-- Date: 13/11/2024
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
 -    Evaluates the degree of a given Poly, using the helper functions:
 -    - isZeroPoly, which, when given a Poly of degree n (unknowing of if the polynomial is a zero polynomial or not),
 -      will evaluate the Poly from [1..n+1] to determine how many roots there are.
 -      If there are more roots than the presupposed degree of the Poly, then it must be a zero polynomial and thus the degree is undefined
 -    - getPolyDegree, which just returns the hard value of the degree without knowledge if the polynomial is zero or not
 -}

{-
- isZeroPoly -------------------------------------------------------
- any polynomial of degree n will have at most n real roots.
- isZeroPoly checks if a function is a zero polynomial by evaluating a Poly using values from [1..n+1],
- and checking to see if they all evaluate to 0 (indicating more roots than degree indicates).
- if they all evaluate to 0, the function returns True
-}
isZeroPoly :: (Num a, Eq a) => Poly a -> Int -> Bool
isZeroPoly _ 0 = True
isZeroPoly (Coef 0) _ = False
isZeroPoly p deg = all (== 0) [polyFun p (fromIntegral x) | x <- [1 .. deg + 1]]

-- calculates the hard value of the degree without knowing if the polynomial is a zero polynomial or not
getPolyDegree :: (Num a, Eq a) => Poly a -> Int
getPolyDegree p = case p of
  Prod (Coef 0) _ -> 0
  Prod _ (Coef 0) -> 0
  Coef _ -> 0
  X -> 1
  Sum p q -> max (getPolyDegree p) (getPolyDegree q)
  Prod p q -> getPolyDegree p + getPolyDegree q

-- if the polynomial is a zero polynomial, an error is returned, otherwise return the degree
polyDegree :: (Num a, Eq a) => Poly a -> Int
polyDegree p = if isZeroPoly p degree then error "Zero polynomial is undefined" else degree
  where
    degree = getPolyDegree p

{- -----------------------------------------------------------------
 - polyListFun
 - -----------------------------------------------------------------
 - Description:
 -    Evaluates a polynomial list representation (PolyList) at some value c
 -}
polyListFun :: (Num a) => PolyList a -> a -> a
polyListFun (PolyList (p : pl)) c = p + c * polyListFun (PolyList pl) c
polyListFun (PolyList []) _ = 0

{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description:
 -    Returns the degree of a PolyList by first omitting trailing zeroes and then finding the length of the properized list
 -}

polyListDegree :: (Num a, Eq a) => PolyList a -> Int
polyListDegree (PolyList []) = error "Zero polynomial is undefined"
polyListDegree (PolyList pl) = if last pl == 0 then polyListDegree (PolyList (init pl)) else length pl - 1

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
 -    Multiplies two polynomials represented by PolyLists by multiplying the second list
 -    by the coefficients in the first, padding the start with as many zeroes as needed to account
 -    for the degree of the monomial being multiplied, and then added to the next multiplication
 -}

polyListProd :: (Num a) => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList []) _ = PolyList [0]
polyListProd (PolyList pl) (PolyList ql) = polyListTermProd `polyListSum` polyListProd (PolyList pl') (PolyList ql)
  where
    coef = last pl -- coefficient of the term currently being multiplied to ql
    pl' = init pl -- the remaining coefficients of terms left to be multiplied to ql
    deg = length pl' -- the degree of the current term is the length of the original list - 1
    polyListTermProd = PolyList ([0 | _ <- [1 .. deg]] ++ map (coef *) ql)

--  polyListTermProd multiplies every element in ql by the coefficient, and pads the start with 0s to increase the degree based on the degree of the multiplier

{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description:
 -    Converts a PolyList representation of a polynomial to that of the Poly type, using the helper functions:
 -    - xExp, which raises X (Poly) to some inputted power
 -}

polyListToPoly :: (Num a) => PolyList a -> Poly a
polyListToPoly (PolyList []) = Coef 0
polyListToPoly (PolyList pl)
  | deg == 0 && not (null pl') = Sum (Coef p) (polyListToPoly (PolyList pl')) -- degree of 0 and more than one term means we add the coefficient to the polynomial
  | deg == 0 = Coef p -- PolyList has one term
  | null pl' = Prod (Coef p) (xExp deg) -- reached the last term in the PolyList (that has more than 1 term)
  | otherwise = Sum (Prod (Coef p) (xExp deg)) (polyListToPoly (PolyList pl')) -- a degree > 0 is the product of the coefficient from the PolyList and X raised to that power, using the helper function xExp
  where
    p = last pl
    pl' = init pl
    deg = length pl'
    -- xExp takes an exponent value >= 0, and raises X to that power using repeated multiplication
    xExp :: (Num a, Ord a) => a -> Poly b
    xExp exp = if exp > 1 then Prod X (xExp (exp - 1)) else X

{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description:
 -    Converts a Poly to a PolyList by simplifying expressions between X and Coef's
 -    using the polyListSum and polyListProd functions
 -}
polyToPolyList :: (Num a, Eq a) => Poly a -> PolyList a
polyToPolyList p = case p of
  Coef x -> PolyList [x]
  X -> PolyList [0, 1]
  Sum p q -> polyToPolyList p `polyListSum` polyToPolyList q
  Prod p q -> polyToPolyList p `polyListProd` polyToPolyList q

{-
Test cases for helper functions were also completed just to ensure that their behaviour followed expected outputs

-- Test Cases for polyFun

-- Function: polyFun
-- Test Case Number: 1
-- Input: polyFun (Coef 5) 2
-- Expected Output: 5
-- Actual Output: 5
-- Rationale: This test verifies that the function correctly evaluates a constant polynomial (a coefficient with no variable terms).

-- Function: polyFun
-- Test Case Number: 2
-- Input: polyFun X 3
-- Expected Output: 3
-- Actual Output: 3
-- Rationale: This test ensures that the function handles the X term correctly, evaluating it as the input value.

-- Function: polyFun
-- Test Case Number: 3
-- Input: polyFun (Sum (Coef 2) X) 4
-- Expected Output: 6
-- Actual Output: 6
-- Rationale: This test evaluates a simple sum of a coefficient and X, ensuring that polyFun correctly sums the evaluated terms.

-- Function: polyFun
-- Test Case Number: 4
-- Input: polyFun (Prod (Sum (Coef 4) X) X) 2
-- Expected Output: 12
-- Actual Output: 12
-- Rationale: This test evaluates a combination of the various sum types involved in the Poly declaration.

-- Test Cases for isZeroPoly

-- Function: isZeroPoly
-- Test Case Number: 1
-- Input: isZeroPoly (Coef 0) 1
-- Expected Output: True
-- Actual Output: True
-- Rationale: This test checks that the function correctly identifies a constant zero coefficient as a zero polynomial.

-- Function: isZeroPoly
-- Test Case Number: 2
-- Input: isZeroPoly (Sum (Prod X X) (Coef 0)) 2
-- Expected Output: False
-- Actual Output: False
-- Rationale: This test verifies that adding a zero coefficient term to a non-zero polynomial does not result in a zero polynomial classification.

-- Function: isZeroPoly
-- Test Case Number: 3
-- Input: isZeroPoly (Prod X (Coef 0)) 2
-- Expected Output: True
-- Actual Output: True
-- Rationale: This test confirms that a product involving a zero coefficient correctly results in a zero polynomial.

-- Function: isZeroPoly
-- Test Case Number: 4
-- Input: isZeroPoly (Sum (Prod X X) (Prod X (Coef 0))) 2
-- Expected Output: False
-- Actual Output: False
-- Rationale: This test confirms that a sum involving a zero polynomial and a nonzero polynomial results in a nonzero polynomial classification.

-- Test Cases for getPolyDegree

-- Function: getPolyDegree
-- Test Case Number: 1
-- Input: getPolyDegree (Coef 7)
-- Expected Output: 0
-- Actual Output: 0
-- Rationale: This test verifies that the function correctly identifies a constant value with a non-zero coefficient as having degree 0.

-- Function: getPolyDegree
-- Test Case Number: 2
-- Input: getPolyDegree X
-- Expected Output: 1
-- Actual Output: 1
-- Rationale: This test ensures that the function correctly identifies a polynomial with a single X term as having degree 1.

-- Function: getPolyDegree
-- Test Case Number: 3
-- Input: getPolyDegree (Prod X (Sum X (Coef 1)))
-- Expected Output: 2
-- Actual Output: 2
-- Rationale: This test evaluates a more complex polynomial, where a product involving two X terms results in a degree of 2, verifying the function's handling of multiplication and sums.

-- Test Cases for polyDegree

-- Function: polyDegree
-- Test Case Number: 1
-- Input: polyDegree (Coef 0)
-- Expected Output: error "Zero polynomial is undefined"
-- Actual Output: error "Zero polynomial is undefined"
-- Rationale: This test verifies that the function correctly raises an error when a zero polynomial is encountered, as its degree is undefined.

-- Function: polyDegree
-- Test Case Number: 2
-- Input: polyDegree (Sum X (Coef 0))
-- Expected Output: 1
-- Actual Output: 1
-- Rationale: This test ensures that adding a zero coefficient term to `X` does not alter the polynomial's degree, verifying proper handling of sums involving zero.

-- Function: polyDegree
-- Test Case Number: 3
-- Input: polyDegree (Prod (Sum X (Coef 2)) X)
-- Expected Output: 2
-- Actual Output: 2
-- Rationale: This test checks the function's ability to evaluate the degree of a product involving terms of different degrees correctly.

-- Function: polyDegree
-- Test Case Number: 4
-- Input: polyDegree (Prod (Coef 0) (Sum X (Coef 1)))
-- Expected Output: error "Zero polynomial is undefined"
-- Actual Output: error "Zero polynomial is undefined"
-- Rationale: This test ensures that a more complex representation of a zero polynomial that still evaluates to an undefined degree.

-- Test Cases for polyListFun

-- Function: polyListFun
-- Test Case Number: 1
-- Input: polyListFun (PolyList [1, 2, 3]) 2
-- Expected Output: 17
-- Actual Output: 17
-- Rationale: This test checks the function's ability to evaluate a polynomial represented as a list [1, 2, 3] (1 + 2x + 3x^2) at c = 2.

-- Function: polyListFun
-- Test Case Number: 2
-- Input: polyListFun (PolyList [0]) 5
-- Expected Output: 0
-- Actual Output: 0
-- Rationale: This test verifies that the function correctly evaluates a zero polynomial as having a result of 0.

-- Function: polyListFun
-- Test Case Number: 3
-- Input: polyListFun (PolyList [4, -3, 0, 1]) 3
-- Expected Output: 22
-- Actual Output: 22
-- Rationale: This test evaluates a polynomial list [4, -3, 0, 1] (4 - 3x + x^3) at c = 3, ensuring correct evaluation with mixed signs and zero coefficients.

-- Function: polyListFun
-- Test Case Number: 4
-- Input: polyListFun (PolyList []) 3
-- Expected Output: 0
-- Actual Output: 0
-- Rationale: This test verifies that an empty list, which is considered a zero polynomial, evaluates to 0.

-- Test Cases for polyListDegree

-- Function: polyListDegree
-- Test Case Number: 1
-- Input: polyListDegree (PolyList [1, 0, 3])
-- Expected Output: 2
-- Actual Output: 2
-- Rationale: This test verifies that the degree is correctly calculated for a PolyList with coefficients of 0 in the middle of the polynomial.

-- Function: polyListDegree
-- Test Case Number: 2
-- Input: polyListDegree (PolyList [0, 0, 0])
-- Expected Output: error "Zero polynomial is undefined"
-- Actual Output: error "Zero polynomial is undefined"
-- Rationale: This test ensures that a PolyList representing a zero polynomial results in an error, as its degree is undefined.

-- Function: polyListDegree
-- Test Case Number: 3
-- Input: polyListDegree (PolyList [5, -3])
-- Expected Output: 1
-- Actual Output: 1
-- Rationale: This test checks the function's ability to evaluate the degree of a polynomial with negative coefficients.

-- Test Cases for polyListSum

-- Function: polyListSum
-- Test Case Number: 1
-- Input: polyListSum (PolyList [1, 2, 3]) (PolyList [4, 5, 6])
-- Expected Output: PolyList [5, 7, 9]
-- Actual Output: PolyList [5, 7, 9]
-- Rationale: This test checks if the function correctly sums the respective coefficients of two PolyLists.

-- Function: polyListSum
-- Test Case Number: 2
-- Input: polyListSum (PolyList [1, 2]) (PolyList [3, 4, 5])
-- Expected Output: PolyList [4, 6, 5]
-- Actual Output: PolyList [4, 6, 5]
-- Rationale: This test evaluates the summing of two PolyLists of different lengths, ensuring the shorter list is correctly concatenated with the longer one after summing.

-- Function: polyListSum
-- Test Case Number: 3
-- Input: polyListSum (PolyList [0, 0, 0]) (PolyList [4, 5, 6])
-- Expected Output: PolyList [4, 5, 6]
-- Actual Output: PolyList [4, 5, 6]
-- Rationale: This test ensures that adding a zero polynomial to another polynomial does not alter the non-zero polynomial.

-- Function: polyListSum
-- Test Case Number: 4
-- Input: polyListSum (PolyList []) (PolyList [4, 5, 6])
-- Expected Output: PolyList [4, 5, 6]
-- Actual Output: PolyList [4, 5, 6]
-- Rationale: This test ensures that adding a zero polynomial (represented via an empty list) to another polynomial does not alter the non-zero polynomial.

-- Test Cases for concatPolyList

-- Function: concatPolyList
-- Test Case Number: 1
-- Input: concatPolyList (PolyList [1, 2]) (PolyList [3, 4])
-- Expected Output: PolyList [1, 2, 3, 4]
-- Actual Output: PolyList [1, 2, 3, 4]
-- Rationale: This test verifies that the function correctly concatenates two PolyLists by combining their respective coefficient lists.

-- Function: concatPolyList
-- Test Case Number: 2
-- Input: concatPolyList (PolyList []) (PolyList [1, 2, 3])
-- Expected Output: PolyList [1, 2, 3]
-- Actual Output: PolyList [1, 2, 3]
-- Rationale: This test checks that an empty PolyList is properly handled when concatenating with a non-empty PolyList, returning the non-empty list.

-- Function: concatPolyList
-- Test Case Number: 3
-- Input: concatPolyList (PolyList [1, 2]) (PolyList [])
-- Expected Output: PolyList [1, 2]
-- Actual Output: PolyList [1, 2]
-- Rationale: This test verifies that concatenating a non-empty PolyList with an empty one correctly returns the non-empty list.

-- Test Cases for polyListProd

-- Function: polyListProd
-- Test Case Number: 1
-- Input: polyListProd (PolyList [1, 2]) (PolyList [3, (-4)])
-- Expected Output: PolyList [3, 2, -8]
-- Actual Output: PolyList [3, 2, -8]
-- Rationale: This test checks that the function correctly multiplies two polynomials represented by PolyLists.

-- Function: polyListProd
-- Test Case Number: 2
-- Input: polyListProd (PolyList [0]) (PolyList [5, 6, 7])
-- Expected Output: PolyList [0, 0, 0]
-- Actual Output: PolyList [0, 0, 0]
-- Rationale: This test verifies that multiplying by a zero polynomial returns a zero polynomial.

-- Function: polyListProd
-- Test Case Number: 3
-- Input: polyListProd (PolyList [1, 0]) (PolyList [3, 4])
-- Expected Output: PolyList [3, 4, 0]
-- Actual Output: PolyList [3, 4, 0]
-- Rationale: This test checks that the function correctly handles a polynomial with a zero coefficient term and performs the multiplication accordingly.

-- Test Cases for polyListToPoly

-- Function: polyListToPoly
-- Test Case Number: 1
-- Input: polyListToPoly (PolyList [1, 2, 3])
-- Expected Output: Sum (Prod (Coef 3) (Prod X X)) (Sum (Prod (Coef 2) X) (Coef 1))
-- Actual Output: Sum (Prod (Coef 3) (Prod X X)) (Sum (Prod (Coef 2) X) (Coef 1))
-- Rationale: This test checks if the function correctly converts a PolyList into a Poly type representation, building the expression as expected.

-- Function: polyListToPoly
-- Test Case Number: 2
-- Input: polyListToPoly (PolyList [0, 0, 1])
-- Expected Output: Sum (Prod (Coef 1) (Prod X X)) (Coef 0)
-- Actual Output: Sum (Prod (Coef 1) (Prod X X)) (Sum (Prod (Coef 0) X) (Coef 0))
-- Rationale: This test verifies that a PolyList where the first two coefficients are zero is correctly translated into an equivalent Poly.

-- Function: polyListToPoly
-- Test Case Number: 3
-- Input: polyListToPoly (PolyList [1])
-- Expected Output: Coef 1
-- Actual Output: Coef 1
-- Rationale: This test checks that a PolyList with a single coefficient (1) is correctly converted into a simple coefficient polynomial, Coef 1.

-- Function: polyListToPoly
-- Test Case Number: 4
-- Input: polyListToPoly (PolyList [])
-- Expected Output: Coef 0
-- Actual Output: Coef 0
-- Rationale: This test checks that an empty PolyList evaluates to a zero polynomial, represented by Coef 0 for simplicity.

-- Test Cases for polyToPolyList

-- Function: polyToPolyList
-- Test Case Number: 1
-- Input: polyToPolyList (Coef 1)
-- Expected Output: PolyList [1]
-- Actual Output: PolyList [1]
-- Rationale: This test checks that the function correctly converts a simple coefficient polynomial into a PolyList with one element.

-- Function: polyToPolyList
-- Test Case Number: 2
-- Input: polyToPolyList (Sum (Coef 1) (Prod (Coef 2) X))
-- Expected Output: PolyList [1, 2]
-- Actual Output: PolyList [1, 2]
-- Rationale: This test ensures that the function correctly handles the conversion of a polynomial that is a sum of a constant and a product of a coefficient and X.

-- Function: polyToPolyList
-- Test Case Number: 3
-- Input: polyToPolyList (Prod (Coef 1) (Prod X X))
-- Expected Output: PolyList [0, 0, 1]
-- Actual Output: PolyList [0, 0, 1]
-- Rationale: This test checks the conversion of a polynomial that is the product of a coefficient and a term involving powers of X.

-- Function: polyToPolyList
-- Test Case Number: 4
-- Input: polyToPolyList (Prod (Sum X (Coef 1)) (Prod X (Sum X (Coef 1))))
-- Expected Output: PolyList [0,1,2,1]
-- Actual Output: PolyList [0,1,2,1]
-- Rationale: This test checks the conversion of a polynomial that is the product of two more complex polynomials in terms of their representation.

-}
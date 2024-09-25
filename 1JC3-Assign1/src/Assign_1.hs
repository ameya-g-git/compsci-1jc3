{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}

-- |
-- Module      : 1JC3-Assign1.Assign_1.hs
-- Copyright   :  (c) Curtis D'Alves 2022
-- License     :  GPL (see the LICENSE file)
-- Maintainer  :  none
-- Stability   :  experimental
-- Portability :  portable
--
-- Description:
--   Assignment 1 - McMaster CS 1JC3 2024.
--
--   Modified by W. M. Farmer 19-SEP-2024.
module Assign_1 where

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS AND DO NOT ADD ANY IMPORTS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: Ameya Gupta
-- Date: 22/09/2024
macid :: String
macid = "gupta67"

-- for reference, a power operator for negative numbers, since powers on these numbers can often result in nonsensical results
(***) :: Double -> Double -> Double
x *** y =
  if x >= 0
    then x ** y
    else -((-x) ** y)

-- calculates the cube root of some Double, x
cbrt :: Double -> Double
cbrt x = x *** (1 / 3)

{- -----------------------------------------------------------------
 - discrim
 - -----------------------------------------------------------------
 - Description:
 -   discrim takes in the coefficients of the cubic and returns the discriminant
 -   an way to see the value of the discriminant without having to type in parameters over and over
 -}
discrim :: Double -> Double -> Double -> Double -> Double
discrim a b c d = (cubicQ a b c) ** 3 + (cubicR a b c d) ** 2

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description:
 -   cubicQ takes in 3 Double parameters - a, b, and c (coefficients in a cubic function) - and returns a Double value that is used to calculate the discriminant of the equation
 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = ((3 * a * c) - (b ** 2)) / (9 * (a ** 2))

{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - Description:
 -   cubicR takes in 4 Double parameters - a, b, c, and d (coefficients in a cubic function) - and returns a Double value that is used in conjunction with the value from cubicQ to calculate the discriminant
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = ((9 * a * b * c) - (27 * (a ** 2) * d) - (2 * (b ** 3))) / (54 * (a ** 3))

{- -----------------------------------------------------------------
 - cubicDiscSign
 - -----------------------------------------------------------------
 - Description:
 -   calculates the discriminant via the results of cubicQ and cubicR, and returns its sign as an integer in { -1, 0, 1 }
 -}
cubicDiscSign :: Double -> Double -> Int
cubicDiscSign q r
  | discrim < 0 = -1
  | abs discrim === 0 = 0
  | otherwise = 1
  where
    discrim = (q ** 3) + (r ** 2)

{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description:
 -   calculates the value S in Cardano's Formula to help determine the roots of the cubic function by using the helper values Q and R from cubicQ and cubicR respectively
 -}
cubicS :: Double -> Double -> Double
cubicS q r = cbrt (r + sqrt discrim)
  where
    discrim = (q ** 3) + (r ** 2)

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description:
 -   calculates the value T in Cardano's Formula to help determine the roots of the cubic function by using the helper values Q and R from cubicQ and cubicR respectively
 -}
cubicT :: Double -> Double -> Double
cubicT q r = cbrt (r - sqrt discrim)
  where
    discrim = (q ** 3) + (r ** 2)

{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description:
 -   takes the coefficients of the cubic as input - a, b, c, and d - and returns a list of the cubic function's real roots if and only if the discriminant is greater than or equal to 0
 -}
cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d
  | a == 0 = []
  | sign == -1 = []
  | sign == 0 = [x1, x2, x2]
  | sign == 1 = [x1]
  | otherwise = []
  where
    sign = cubicDiscSign q r
    s = cubicS q r
    t = cubicT q r
    q = cubicQ a b c
    r = cubicR a b c d
    x1 = s + t - (b / (3 * a))
    x2 = -((s + t) / 2) - (b / (3 * a))

(===) :: Double -> Double -> Bool
x === y =
  let tol = 1e-3
   in abs (x - y) <= tol

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -}

-- TODO Add Test Cases for each of your functions below here

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------}
testCubicQ1 = cubicQ 1 2 3 === 0.55556

testCubicQ2 = cubicQ (-1) 2 (-3) === 0.55556

testCubicQ3 = cubicQ 1.5 2.3 3.4 === 0.49432

{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------}
testCubicR1 = cubicR 1 2 3 4 === (-1.29630)

testCubicR2 = cubicR (-1) 2 (-3) 4 === 1.29630

testCubicR3 = cubicR 1.5 2.3 3.4 4.2 === (-0.95426)

{- -----------------------------------------------------------------
 - cubicDiscSign
 - -----------------------------------------------------------------}
testCubicDiscSign1 = cubicDiscSign 1 2 == 1

testCubicDiscSign2 = cubicDiscSign (-1) 0 == (-1)

testCubicDiscSign3 = cubicDiscSign 0 0 == 0

{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------}
testCubicS1 = cubicS 1 2 === 1.61803

testCubicS2 = cubicS 4 0 === 2.0

testCubicS3 = cubicS 0 0 === 0.0

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------}
testCubicT1 = cubicT 1 2 === (-0.61803)

testCubicT2 = cubicT 0 0 === 0.0

{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------}
testCubicRealSolutions1 = cubicRealSolutions 1 0 0 0 == [0.0, 0.0, 0.0] -- discriminant = 0, x2 = x3, and x1 = x2 = x3

testCubicRealSolutions2 = null (cubicRealSolutions 1 0 (-1) 0) -- negative discriminant, no output

testCubicRealSolutions3 = cubicRealSolutions 1 (-6) 11 6 == [-0.43484136821690855] -- Three distinct real roots

testList :: [Bool]
testList = [testCubicQ1, testCubicQ2, testCubicQ3, testCubicR1, testCubicR2, testCubicR3, testCubicDiscSign1, testCubicDiscSign2, testCubicDiscSign3, testCubicS1, testCubicS2, testCubicS3, testCubicT1, testCubicT2, testCubicRealSolutions1, testCubicRealSolutions2, testCubicRealSolutions3]

runTests :: [Bool] -> IO ()
runTests (x : xs) = (if x then putStrLn "Test Passed" else putStrLn "Test failed") >> runTests xs
runTests [] = putStrLn "End of tests"

main :: IO ()
main = runTests testList

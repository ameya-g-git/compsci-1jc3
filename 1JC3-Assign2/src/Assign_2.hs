{-|
Module      : 1JC3-Assign2.Assign_2.hs
Copyright   :  (c) William M. Farmer 2024
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 2 - McMaster CS 1JC3 2024
-}
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

-- Name: TODO add name
-- Date: TODO add date
macid :: String
macid = "TODO"

type GaussianInt = (Integer,Integer)

{- -----------------------------------------------------------------
 - gaussReal
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussReal :: GaussianInt -> Integer
gaussReal x = error "TODO implement gaussReal"

{- -----------------------------------------------------------------
 - gaussImag
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussImag :: GaussianInt -> Integer
gaussImag x = error "TODO implement gaussImag"

{- -----------------------------------------------------------------
 - gaussConj
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussConj :: GaussianInt -> GaussianInt
gaussConj x = error "TODO implement gaussConj"

{- -----------------------------------------------------------------
 - gaussAdd
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussAdd :: GaussianInt -> GaussianInt -> GaussianInt
gaussAdd x y = error "TODO implement gaussAdd"


{- -----------------------------------------------------------------
 - gaussMul
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussMul :: GaussianInt -> GaussianInt -> GaussianInt
gaussMul x y = error "TODO implement gaussMul"


{- -----------------------------------------------------------------
 - gaussNorm
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussNorm :: GaussianInt -> Integer
gaussNorm x = error "TODO implement gaussNorm"

{- -----------------------------------------------------------------
 - gaussAddList
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussAddList :: [GaussianInt] -> [GaussianInt] -> GaussianInt
gaussAddList x y = error "TODO implement gaussAddList"

{- -----------------------------------------------------------------
 - gaussMulList
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussMulList :: [GaussianInt] -> [GaussianInt] -> GaussianInt
gaussMulList x y = error "TODO implement gaussMulList"

{- ------------------------------------------------------------------------
 - gaussCircle
 - ------------------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussCircle :: [GaussianInt] -> Integer -> [GaussianInt]
gaussCircle vs = error "TODO implement gaussCircle"

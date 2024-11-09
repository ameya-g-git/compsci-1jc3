{-|
Module      : 1JC3-Assign3.Assign_3.hs
Copyright   :  (c) William M. Farmer 2024
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 3 - McMaster CS 1JC3 2024
-}
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

-- Name: TODO add name
-- Date: TODO add date
macid :: String
macid = "TODO"

{- -----------------------------------------------------------------
 - Datatypes
 - -----------------------------------------------------------------
 -}

data Poly a = 
    X
  | Coef a
  | Sum (Poly a) (Poly a)
  | Prod (Poly a) (Poly a)
  deriving Show

newtype PolyList a = PolyList [a]
  deriving Show

{- -----------------------------------------------------------------
 - polyFun
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
polyFun :: Num a => Poly a -> a -> a
polyFun p c = error "TODO implement polyFun"

{- -----------------------------------------------------------------
 - polyDegree
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
polyDegree :: (Num a,Eq a) => Poly a -> Int
polyDegree p = error "TODO implement polyDegree"

{- -----------------------------------------------------------------
 - polyListFun
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
polyListFun :: Num a => PolyList a -> a -> a
polyListFun pl c = error "TODO implement polyListFun"

{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
polyListDegree :: (Num a,Eq a) => PolyList a -> Int
polyListDegree pl = error "TODO implement polyListDegree"

{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
polyListSum :: Num a => PolyList a -> PolyList a -> PolyList a
polyListSum pl ql = error "TODO implement polyListSum"

{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
polyListProd :: Num a => PolyList a -> PolyList a -> PolyList a
polyListProd pl ql = error "TODO implement polyListProd"

{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly pl = error "TODO implement polyListToPoly"

{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
polyToPolyList :: Num a => Poly a -> PolyList a
polyToPolyList p = error "TODO implement polyToPolyList"


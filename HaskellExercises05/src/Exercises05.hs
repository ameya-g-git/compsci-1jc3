-- |
-- Module      : HaskellExercises05.Exercises05
-- Copyright   :  (c) Curtis D'Alves 2020
-- License     :  GPL (see the LICENSE file)
-- Maintainer  :  none
-- Stability   :  experimental
-- Portability :  portable
--
-- Description:
--   Haskell exercise template Set 05 - McMaster CS 1JC3 2021
module Exercises05 where

import Prelude hiding (and, drop, elem, or, replicate, take, (!!))

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

-- Exercise A
-----------------------------------------------------------------------------------------------------------
-- Implement the function split that takes a list and splits it in half and returns a tuple of the
-- two halves, WITHOUT USING TAKE / DROP
-- NOTE when the list is uneven, the second list is one element larger than the first
-- NOTE^2 when using take / drop, although convenient, you introduce redundant computation. A more
--        efficient implementation of this function can be done calling an auxilary function with
--        different parameters that recurses through the list directly
-----------------------------------------------------------------------------------------------------------
split :: [a] -> ([a], [a])
split xs =
  let half = length xs `div` 2
      split' :: [a] -> [a] -> Int -> ([a], [a])
      split' xs (y : ys) n = if n > 0 then split' (xs ++ [y]) ys (n - 1) else (xs, y : ys)
      split' xs [] n = (xs, [])
   in split' [] xs half

-- Exercise B
-----------------------------------------------------------------------------------------------------------
-- Implement the function merge that takes two lists that (assuming both lists are already sorted) merges
-- them together in to a sorted list
-----------------------------------------------------------------------------------------------------------
merge :: (Ord a) => [a] -> [a] -> [a]
merge (x : xs) (y : ys)
  | x >= y = y : merge (x : xs) ys
  | otherwise = x : merge xs (y : ys)
merge xs ys = xs ++ ys -- one list has no items

-- Exercise C
-----------------------------------------------------------------------------------------------------------
-- Implement the function mergeSort that sorts a list by recusively splitting a list, and merging
-- the sorted lists back together
-- NOTE singleton and empty lists are already sorted
-----------------------------------------------------------------------------------------------------------
mergeSort :: (Ord a) => [a] -> [a]
mergeSort xs = mergeSortAux (split xs)
  where
    mergeSortAux :: (Ord a) => ([a], [a]) -> [a]
    mergeSortAux (l, r)
      | length l > 1 || length r > 1 = merge (mergeSortAux (split l)) (mergeSortAux (split r))
      | otherwise = merge l r

-- mergeSort xs
--   | length xs > 2 = merge (mergeSort l) (mergeSort r) -- recursively split if list is greater than 2 items
--   | length xs == 2 = merge [head xs] (tail xs) -- sort lists with 2 items
--   | otherwise = xs -- singleton / empty list is sorted
--   where
--     (l, r) = split xs

-- Exercise D
-----------------------------------------------------------------------------------------------------------
-- Implement the function sortProp that tests if a list is sorted or not
-- NOTE you can use this with QuickCheck to test your mergSort function by calling
--      quickCheck (sortProp . mergeSort)
-----------------------------------------------------------------------------------------------------------
sortProp :: (Ord a) => [a] -> Bool
-- sortProp [] = True
-- sortProp (x : xs) = sortPropAux x xs
--   where
--     sortPropAux :: (Ord a) => a -> [a] -> Bool
--     sortPropAux x (y : ys) = (x <= y) && sortPropAux y ys
--     sortPropAux _ [] = True

sortProp (x : y : ys) = x <= y && sortProp (y : ys)
sortProp xs = True

-- Exercise E
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude function replicate that takes an Int n and a element and returns a list that
-- replicates that element n times
-----------------------------------------------------------------------------------------------------------
replicate :: Int -> a -> [a]
replicate n x = if n > 0 then x : replicate (n - 1) x else []

-- Exercise F
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude function !! that selects the nth element of a list using recursion
-- NOTE throw an error when indexing out of bounds
-----------------------------------------------------------------------------------------------------------
(!!) :: [a] -> Int -> a
(!!) (x : xs) n
  | length (x : xs) < n || n < 0 = error "Index out of bounds."
  | otherwise = if n > 0 then (!!) xs (n - 1) else x

-- Exercise G
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude function elem that takes a value and a list and returns True if the value
-- is an element of the list
-----------------------------------------------------------------------------------------------------------
elem :: (Eq a) => a -> [a] -> Bool
elem e (x : xs) = e == x || elem e xs
elem _ [] = False

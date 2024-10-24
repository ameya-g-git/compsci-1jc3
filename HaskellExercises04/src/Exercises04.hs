-- |
-- Module      : HaskellExercises04.Exercises04
-- Copyright   :  (c) Curtis D'Alves 2020
-- License     :  GPL (see the LICENSE file)
-- Maintainer  :  none
-- Stability   :  experimental
-- Portability :  portable
--
-- Description:
--   Haskell exercise template Set 04 - McMaster CS 1JC3 2021
module Exercises04 where

import Prelude hiding (drop, take, zip)

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
-- Implement the Prelude function zip that takes two lists and combines them into a list of pairs
-- E.x.
--  zip [1,2,3] ['a','b'] == [(1,'a'),(2,'b')]
-- NOTE zip short circuits on the shortest list
-----------------------------------------------------------------------------------------------------------
zip :: [a] -> [b] -> [(a, b)]
zip xs ys = if null xs || null ys then [] else (head xs, head ys) : zip (tail xs) (tail ys)

-- Exercise B
-----------------------------------------------------------------------------------------------------------
-- Implement the function mapWithIndex, that works just like map, however the function it "maps" takes
-- a tuple containing the index of each element of the list
-- E.x.
--  mapWithIndex (\(idx,x) -> idx + x) [0,0,0] == [0,1,2]
-- NOTE zip [0..] xs   creates a list of (index,element) tuples
-----------------------------------------------------------------------------------------------------------
mapWithIndex :: ((Int, a) -> b) -> [a] -> [b]
mapWithIndex f xs =
  let indexElemList = zip [0 ..] xs
      mapWithIndexAux :: ((Int, a) -> b) -> [(Int, a)] -> [b]
      mapWithIndexAux _ [] = []
      mapWithIndexAux f idxList = f (head idxList) : mapWithIndexAux f (tail idxList)
   in mapWithIndexAux f indexElemList

-- Exercise C
-----------------------------------------------------------------------------------------------------------
-- Implement the function mySum that works just like the Prelude function sum that adds up all the elements
-- in a list, but works on the following custom List data type instead
-----------------------------------------------------------------------------------------------------------
data List a
  = Cons a (List a)
  | Nil
  deriving (Show, Eq)

mySum :: (Num a) => List a -> a
mySum xs = case xs of
  Cons x list -> x + mySum list
  Nil -> 0

-- Exercise D
-----------------------------------------------------------------------------------------------------------
-- Implement the operator +++ that works just like the Prelude operator ++ that puts two list together
-- (i.e. concatenates them), but works on the above custom List data type instead
-----------------------------------------------------------------------------------------------------------
(+++) :: List a -> List a -> List a
xs +++ ys = case xs of
  Cons x list -> Cons x (list +++ ys)
  Nil -> ys

-- Exercise E
-----------------------------------------------------------------------------------------------------------
-- Implement the function myReverse that works just like the Prelude function reverse that reverses the
-- order of all elements in a list, but works on the above custom List data type instead
-----------------------------------------------------------------------------------------------------------
myReverse :: List a -> List a
myReverse xs = case xs of
  Cons x list -> myReverse list +++ Cons x Nil -- because +++ replaces the former Nil with the list in front of the operator
  Nil -> Nil

-- Exercise F
-----------------------------------------------------------------------------------------------------------
-- Implement the function treeSum that works just like the Prelude function sum that adds up all the
-- elements in a list, but works on the following custom Tree data type instead
-----------------------------------------------------------------------------------------------------------
data Tree a
  = Node a (Tree a) (Tree a)
  | Empty
  deriving (Show, Eq)

treeSum :: (Num a) => Tree a -> a
treeSum tree = case tree of
  Node val left right -> val + treeSum left + treeSum right
  Empty -> 0

-- Exercise G
-----------------------------------------------------------------------------------------------------------
-- Implement the function treeHeight that returns the largest height of a Tree
-- E.x.
--              a
--            /  \
--           b    c
--               / \
--              d   e
-- has a height of 3 (elements d and e are both at "height" 3 in the tree)
-- NOTE the Empty Tree is of height 0
-----------------------------------------------------------------------------------------------------------
treeHeight :: Tree a -> Int
treeHeight tree = case tree of
  Node _ left right -> 1 + max (treeHeight left) (treeHeight right)
  Empty -> 0

-- Exercise H
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude functions take and drop that take / drop the first n elements of a list
-----------------------------------------------------------------------------------------------------------
take :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n xs = if n > 0 then head xs : take (n - 1) (tail xs) else []

drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 xs = xs
drop n xs = if n < 0 then head xs : drop (n - 1) (tail xs) else drop (n - 1) (tail xs)

-- Extra Challenge
-----------------------------------------------------------------------------------------------------------
-- Try and create a QuickCheck property that tests take and drop in combination
-- See the function takeDropProp in app/Main.hs for a solution
-----------------------------------------------------------------------------------------------------------

takeDropProp :: Int -> [Int] -> Bool
takeDropProp n xs = take n xs ++ drop n xs == xs

-- main = quickCheck takeDropProp

-- i dont have the QuickCheck library installed so this throws an error
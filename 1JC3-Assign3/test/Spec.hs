{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Assign_3 (PolyList (PolyList))
import qualified Assign_3 as A3
import Test.Hspec
import Test.QuickCheck (Gen, Positive (..), Result (Success), Testable, maxSuccess, quickCheck, quickCheckResult, quickCheckWithResult, stdArgs, within)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (sized)
import Test.QuickCheck.Property (property)

------------------------------------------------------------------------------------
-------

-- * QuickCheck Tests

-- | Existential type wrapper for QuickCheck propositions, allows @propList@ to essentially
--   act as a heterogeneous list that can hold any quickcheck propositions of any type
data QuickProp
  = forall prop.
  (Testable prop) =>
  QuickProp
  { quickPropName :: String,
    quickPropMark :: Int,
    quickPropFunc :: prop
  }

instance (Arbitrary a) => Arbitrary (A3.Poly a) where
  arbitrary =
    let genTree n
          | n <= 0 = return A3.X
          | otherwise = do
              (Positive n0) <- arbitrary
              t0 <- genTree (n `div` (n0 + 1)) -- generate an arbitrary sized left tree
              (Positive n1) <- arbitrary
              t1 <- genTree (n `div` (n1 + 1)) -- generate an arbitrary sized right tree
              f <- arbitrary :: Gen Bool
              f2 <- arbitrary :: Gen Bool
              i <- arbitrary
              return $
                if f
                  then
                    (if f2 then A3.Sum t0 t1 else A3.Prod t0 t1)
                  else A3.Coef i
     in sized genTree

ddx xs = zipWith (-) (tail xs) xs

chain :: Int -> (a -> a) -> a -> a
chain 0 f x = x
chain n f x = f (chain (n - 1) f x)

polyListFunProp0 :: (Int, Int) -> Bool
polyListFunProp0 (a, x) = a == A3.polyListFun (PolyList [a]) x

polyListFunProp1 :: (Int, Int, Int) -> Bool
polyListFunProp1 (a, b, x) = a + b * x == A3.polyListFun (PolyList [a, b]) x

polyListFunProp2 :: (Int, Int, Int, Int) -> Bool
polyListFunProp2 (a, b, c, x) = a + b * x + c * x * x == A3.polyListFun (PolyList [a, b, c]) x

polyListFunProp3 :: (Int, Int, Int, Int, Int) -> Bool
polyListFunProp3 (a, b, c, d, x) = a + b * x + c * x * x + d * x * x * x == A3.polyListFun (PolyList [a, b, c, d]) x

polyListDegreeProp1 :: [Integer] -> Bool
polyListDegreeProp1 xs | all (== 0) xs = True
polyListDegreeProp1 xs = 0 `notElem` dndx
  where
    mapped = fmap (A3.polyListFun (PolyList xs)) [1 .. (2 * fromIntegral (length xs))]
    deg = A3.polyListDegree (PolyList xs)
    dndx = chain deg ddx mapped

polyListDegreeProp2 :: [Integer] -> Bool
polyListDegreeProp2 xs = all (== 0) dndx
  where
    mapped = fmap (A3.polyListFun (PolyList xs)) [1 .. (2 * fromIntegral (length xs))]
    deg = A3.polyListDegree (PolyList xs)
    dndx = chain (deg + 1) ddx mapped

polyListSumProp :: ([Int], [Int], Int) -> Bool
polyListSumProp (p, q, x) = a == b
  where
    a = A3.polyListFun (PolyList p) x + A3.polyListFun (PolyList q) x
    b = A3.polyListFun (A3.polyListSum (PolyList p) (PolyList q)) x

polyListProdProp :: ([Int], [Int], Int) -> Bool
polyListProdProp ([], _, x) = True
polyListProdProp (_, [], x) = True
polyListProdProp (p, q, x) = a == b
  where
    a = A3.polyListFun (PolyList p) x * A3.polyListFun (PolyList q) x
    b = A3.polyListFun (A3.polyListProd (PolyList p) (PolyList q)) x

polyDegreeProp1 :: A3.Poly Integer -> Bool
polyDegreeProp1 p
  | all (== 0) mapped = True
  | otherwise = 0 `notElem` dndxn
  where
    mapped = fmap (A3.polyFun p) [1 .. (10 + 2 * fromIntegral (deg * 2))]
    deg = A3.polyDegree p
    dndxn = chain deg ddx mapped

polyDegreeProp2 :: A3.Poly Integer -> Bool
polyDegreeProp2 p = all (== 0) dndxn
  where
    mapped = fmap (A3.polyFun p) [1 .. (10 + 2 * fromIntegral deg)]
    deg = A3.polyDegree p
    dndxn = chain (deg + 1) ddx mapped

polyToPolyListProp1 :: A3.Poly Integer -> Bool
polyToPolyListProp1 p = and . zipWith (==) mappedP $ mappedPl
  where
    mappingRange = [-50 .. 50]
    pl = A3.polyToPolyList p
    mappedP = fmap (A3.polyFun p) mappingRange
    mappedPl = fmap (A3.polyListFun pl) mappingRange

polyListToPolyProp1 :: [Integer] -> Bool
polyListToPolyProp1 xs = and . zipWith (==) mappedP $ mappedPl
  where
    pl = A3.PolyList xs
    mappingRange = [-50 .. 50]
    p = A3.polyListToPoly pl
    mappedP = fmap (A3.polyFun p) mappingRange
    mappedPl = fmap (A3.polyListFun pl) mappingRange

polyConversionInverseProp :: [Integer] -> Bool
polyConversionInverseProp xs
  | null xs = True
  | last xs == 0 = True
  | otherwise = xs == ys
  where
    pl = A3.PolyList xs
    (PolyList ys) = A3.polyToPolyList . A3.polyListToPoly $ pl

main :: IO ()
main = hspec $ do
  describe "polyListFun" $
    it "should be constant for single coefficients" $
      property polyListFunProp0
  describe "polyListFun" $
    it "should be linear for two coefficients" $
      property polyListFunProp1
  describe "polyListFun" $
    it "should be quadratic for three coefficients" $
      property polyListFunProp2
  describe "polyListFun" $
    it "should be cubic for four coefficients" $
      property polyListFunProp3
  describe "polyListDegree" $
    it "the nth derivative should not be zero" $
      property polyListDegreeProp1
  describe "polyListDegree" $
    it "the (n+1)th derivative should be zero" $
      property polyListDegreeProp1
  describe "polyListSum" $
    it "evaluating a polyList then adding should be the same as evaluating the sum of two polyLists" $
      property polyListSumProp
  describe "polyListProd" $
    it "evaluating a polyList then multiplying should be the same as evaluating the products of two polyLists" $
      property polyListProdProp
  describe "polyDegree" $
    it "the nth derivative should not be zero" $
      property polyDegreeProp1
  describe "polyDegree" $
    it "the (n+1)th derivative should be zero" $
      property polyDegreeProp2
  describe "polyToPolyList" $
    it "the function should map values for c to the same value after being converted" $
      property polyToPolyListProp1
  describe "polyListToPoly" $
    it "the function should map values for c to the same value after being converted" $
      property polyListToPolyProp1
  describe "polyToPolyList,polyListToPoly" $
    it "converting polyList to poly then back to polyList should result in the same polyList" $
      property polyConversionInverseProp

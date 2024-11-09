
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import qualified Assign_2 as A2
import Data.Complex

import Test.QuickCheck (quickCheck
                       ,quickCheckResult
                       ,quickCheckWithResult
                       ,stdArgs
                       ,maxSuccess
                       ,Result(Success)
                       ,within
                       ,Testable)
import Test.Hspec
import Test.QuickCheck.Property (property)
import Assign_2 (gaussAddList)

data QuickProp = forall prop . Testable prop =>
                 QuickProp { quickPropName :: String
                           , quickPropMark :: Int
                           , quickPropFunc :: prop
                           }

j = fromIntegral
f :: Integral a => a -> Double
f = fromIntegral
m n x = j (x `mod` n - n `div` 2)

toGauss :: (Int,Int) -> (Integer, Integer)
toGauss (a,b) = (j a, j b)


conjProp :: (Int,Int) -> Bool
conjProp (r',i') = ar == realPart conj && ai == imagPart conj
    where
    (r,i) = (j r', j i')
    conj = conjugate (r :+ i)
    (ar,ai) = A2.gaussConj (r,i)

addProp :: (Int,Int,Int,Int) -> Bool
addProp (a',b',c',d') = r == toInteger (realPart sum) && i == toInteger (imagPart sum)
    where
    (a,b,c,d) = (m 400 a',m 400 b',m 400 c',m 400 d')
    (r,i) = A2.gaussAdd (a,b) (c,d)
    sum' :: Complex Double
    sum' = (f a :+ f b) + (f c :+ f d)
    sum = round (realPart sum') :+ round (imagPart sum')

mulProp :: (Int,Int,Int,Int) -> Bool
mulProp (a',b',c',d') = r == realPart prod && i == imagPart prod
    where
    (a,b,c,d) = (m 400 a',m 400 b',m 400 c',m 400 d')
    (r,i) = A2.gaussMul (a,b) (c,d)
    prod' :: Complex Double
    prod' = (f a :+ f b) * (f c :+ f d)
    prod = round (realPart prod') :+ round (imagPart prod')

addListProp :: [(Int,Int)] -> Bool
addListProp xs = sum == sum2
    where
    l = fmap toGauss xs
    sum = foldl A2.gaussAdd (0,0) l
    sum2 = A2.gaussAddList l

mulListProp :: [(Int,Int)] -> Bool
mulListProp xs = prod == prod2
    where
    l = fmap toGauss xs
    prod = foldl A2.gaussMul (1,0) l
    prod2 = A2.gaussMulList l

normProp :: (Int,Int) -> Bool
normProp (r',i') = realPart norm == n && imagPart norm == 0
    where
    (r,i) = (m 400 r', m 400 i')
    norm' = (f r :+ f i) * conjugate (f r :+ f i)
    norm = round (realPart norm') :+ round (imagPart norm')
    n = A2.gaussNorm (r,i)

circProp :: ([(Int,Int)],Int) -> Bool
circProp (xs, n') = circles == circles2
    where
    n = fromIntegral n'
    circles = filter ((<n) . A2.gaussNorm) . fmap toGauss $ xs
    circles2 = A2.gaussCircle (fmap toGauss xs) n

main = hspec $ do
  describe "gaussConj" $
      it "should be the conjugate" $ property conjProp
  describe "gaussAdd" $
      it "should be complex addition" $ property addProp
  describe "gaussMul" $
      it "should be complex multiplication" $ property mulProp
  describe "gaussNorm" $
      it "should be complex normal" $ property normProp
  describe "gaussAddList" $
      it "should add a list" $ property addListProp
  describe "gaussMulList" $
      it "should multiply a list" $ property mulListProp
  describe "gaussCircle" $
      it "should filtered points" $ property circProp

module Section_3.Lesson_4.Step_3_4_3Spec where

import Data.Functor.Identity
import Section_3.Lesson_4.Step_3_4_3
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "Arr2T & Arr3T" $ do
    it "(getArr2T $ arr2 (+)) 33 9 :: [Integer]" $
      do
        (getArr2T $ arr2 (+)) 33 9 :: [Integer]
        `shouldBe` [42]
    it "(getArr3T $ arr3 foldr) (*) 1 [1..5] :: Either String Integer" $
      do
        (getArr3T $ arr3 foldr) (*) 1 [1 .. 5] :: Either String Integer
        `shouldBe` Right 120
    it "runIdentity $ (getArr2T $ arr2 (+)) 33 9" $
      do
        runIdentity $ (getArr2T $ arr2 (+)) 33 9
        `shouldBe` (42 :: Integer)

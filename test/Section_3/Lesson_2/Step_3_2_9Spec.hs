module Section_3.Lesson_2.Step_3_2_9Spec where

import Section_3.Lesson_2.Step_3_2_9
import Test.Hspec

addTens :: Int -> Checkpointed Int
addTens x1 checkpoint = do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2
  let x3 = x2 + 10
  checkpoint x3
  let x4 = x3 + 10
  return x4

spec :: Spec
spec = parallel $ do
  describe "Cont: runCheckpointed" $ do
    it "runCheckpointed (< 100) $ addTens 1" $
      do
        runCheckpointed (< 100) $ addTens 1
        `shouldBe` (31 :: Int)

    it "runCheckpointed  (< 30) $ addTens 1" $
      do
        runCheckpointed (< 30) $ addTens 1
        `shouldBe` (21 :: Int)

    it "runCheckpointed  (< 20) $ addTens 1" $
      do
        runCheckpointed (< 20) $ addTens 1
        `shouldBe` (11 :: Int)

    it "runCheckpointed  (< 10) $ addTens 1" $
      do
        runCheckpointed (< 10) $ addTens 1
        `shouldBe` (1 :: Int)

    it "runCheckpointed   (< 0) $ addTens 1" $
      do
        runCheckpointed (< 0) $ addTens 1
        `shouldBe` (1 :: Int)

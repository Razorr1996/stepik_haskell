module Section_3.Lesson_3.Step_3_3_11Spec where

import Control.Monad
import Section_3.Lesson_3.Step_3_3_11
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "EsSi" $ do
    it "runEsSi (go 1 85 tickCollatz) 27" $
      do
        runEsSi (go 1 85 tickCollatz) 27
        `shouldBe` ((Right (), 82) :: (Either String (), Integer))

    it "runEsSi (go 1 80 tickCollatz) 27" $
      do
        runEsSi (go 1 80 tickCollatz) 27
        `shouldBe` ((Left "Upper bound", 82) :: (Either String (), Integer))

    it "runEsSi (forever $ go 1 1000 tickCollatz) 27" $
      do
        runEsSi (forever $ go 1 1000 tickCollatz) 27
        `shouldBe` ((Left "Upper bound", 1186) :: (Either String (), Integer))

    it "runEsSi (forever $ go 1 10000 tickCollatz) 27" $
      do
        runEsSi (forever $ go 1 10000 tickCollatz) 27
        `shouldBe` ((Left "Lower bound", 1) :: (Either String (), Integer))

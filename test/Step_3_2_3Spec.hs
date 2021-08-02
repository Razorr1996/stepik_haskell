module Step_3_2_3Spec where

import Step_3_2_3
import Test.Hspec

spec :: Spec
spec = do
  describe "CPS" $ do
    it "decode one as a number" $ do
      decode one as a number `shouldBe` (1 :: Integer)

    it "decode one hundred twenty three as a number" $ do
      decode one hundred twenty three as a number `shouldBe` (123 :: Integer)

    it "decode one hundred twenty one as a number" $ do
      decode one hundred twenty one as a number `shouldBe` (121 :: Integer)

    it "decode one hundred twenty as a number" $ do
      decode one hundred twenty as a number `shouldBe` (120 :: Integer)

    it "decode one hundred as a number" $ do
      decode one hundred as a number `shouldBe` (100 :: Integer)

    it "decode three hundred as a number" $ do
      decode three hundred as a number `shouldBe` (300 :: Integer)

    it "decode two thousand seventeen as a number" $ do
      decode two thousand seventeen as a number `shouldBe` (2017 :: Integer)

module Step_4_5_3Spec where

import Step_4_5_2
import Step_4_5_3
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "MonadError: treeSum" $ do
    it "treeSum $ Fork (Fork (Leaf \"1\") \"2\" (Leaf \"oops\")) \"15\" (Leaf \"16\")" $
      do
        treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "oops")) "15" (Leaf "16")
        `shouldBe` Left (NoParse "oops")

    it "treeSum $ Fork (Fork (Leaf \"1\") \"2\" (Leaf \"0\")) \"15\" (Leaf \"16\")" $
      do
        treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "0")) "15" (Leaf "16")
        `shouldBe` Right 34

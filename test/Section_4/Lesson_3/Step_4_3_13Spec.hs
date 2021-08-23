module Section_4.Lesson_3.Step_4_3_13Spec where

import Section_3.Lesson_1.Step_3_1_8 (ReadError (..))
import Section_4.Lesson_3.Step_4_3_13
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "ExceptT: Tree -> treeSum -> go" $ do
    it "treeSum $ Fork (Fork (Leaf \"1\") \"2\" (Leaf \"oops\")) \"15\" (Leaf \"16\")" $
      do
        treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "oops")) "15" (Leaf "16")
        `shouldBe` (Just (NoParse "oops"), 3)

    it "treeSum $ Fork (Fork (Leaf \"1\") \"2\" (Leaf \"0\")) \"15\" (Leaf \"16\")" $
      do
        treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "0")) "15" (Leaf "16")
        `shouldBe` (Nothing, 34)

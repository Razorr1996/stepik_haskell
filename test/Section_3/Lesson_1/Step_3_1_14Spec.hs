module Section_3.Lesson_1.Step_3_1_14Spec where

import Section_3.Lesson_1.Step_3_1_14
import Section_3.Lesson_1.Step_3_1_8
import Section_3.Lesson_1.Step_3_1_9

import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "Validate" $ do
    it "getValidate $ validateSum [\"10\", \"20\", \"30\"]" $
      do
        getValidate $ validateSum ["10", "20", "30"]
        `shouldBe` Right 60

    it "getValidate $ validateSum [\"10\", \"\", \"30\", \"oops\"]" $
      do
        getValidate $ validateSum ["10", "", "30", "oops"]
        `shouldBe` Left [SumError 2 EmptyInput, SumError 4 (NoParse "oops")]

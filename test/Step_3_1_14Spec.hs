module Step_3_1_14Spec where

import Step_3_1_14
import Step_3_1_8
import Step_3_1_9

import Test.Hspec

spec :: Spec
spec = do
  describe "Validate" $ do
    it "getValidate $ validateSum [\"10\", \"20\", \"30\"]" $
      do
        getValidate $ validateSum ["10", "20", "30"]
        `shouldBe` Right 60

    it "getValidate $ validateSum [\"10\", \"\", \"30\", \"oops\"]" $
      do
        getValidate $ validateSum ["10", "", "30", "oops"]
        `shouldBe` Left [SumError 2 EmptyInput, SumError 4 (NoParse "oops")]

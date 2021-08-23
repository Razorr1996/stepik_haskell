module Section_3.Lesson_1.Step_3_1_9Spec where

import Control.Monad.Trans.Except (runExcept)
import Section_3.Lesson_1.Step_3_1_8
import Section_3.Lesson_1.Step_3_1_9
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "trySum" $ do
    it "runExcept $ trySum [\"10\", \"20\", \"30\"]" $
      do
        runExcept $ trySum ["10", "20", "30"]
        `shouldBe` Right 60

    it "runExcept $ trySum [\"10\", \"20\", \"\"]" $
      do
        runExcept $ trySum ["10", "20", ""]
        `shouldBe` Left (SumError 3 EmptyInput)

    it "runExcept $ trySum [\"10\", \"two\", \"30\"]" $
      do
        runExcept $ trySum ["10", "two", "30"]
        `shouldBe` Left (SumError 2 (NoParse "two"))

module Section_3.Lesson_3.Step_3_3_10Spec where

import Data.Char
import Section_3.Lesson_3.Step_3_3_10
import Section_3.Lesson_3.Step_3_3_9
import Test.Hspec

logFirstAndRetSecond :: MyRWT Maybe String
logFirstAndRetSecond = do
  xs <- myAsk
  case xs of
    (el1 : el2 : _) -> myTell el1 >> return (map toUpper el2)
    _ -> myLift Nothing

strings :: [String]
strings = ["abc", "defg", "hij"]

strings1 :: [String]
strings1 = ["abc", "defg", "hij", "kl"]

spec :: Spec
spec = parallel $ do
  describe "logFirstAndRetSecond" $ do
    it "runMyRWT logFirstAndRetSecond [\"abc\",\"defg\",\"hij\"]" $ do
      runMyRWT logFirstAndRetSecond strings `shouldBe` Just ("DEFG", "abc")

    it "runMyRWT logFirstAndRetSecond [\"abc\"]" $ do
      runMyRWT logFirstAndRetSecond ["abc"] `shouldBe` Nothing

  describe "veryComplexComputation" $ do
    it "runMyRWT veryComplexComputation [\"abc\",\"defg\",\"hij\"]" $ do
      runMyRWT veryComplexComputation strings `shouldBe` Nothing

    it "runMyRWT veryComplexComputation [\"abc\",\"defg\",\"hij\",\"kl\"]" $ do
      runMyRWT veryComplexComputation strings1 `shouldBe` Just (("KL", "HIJ"), "defg,abc")

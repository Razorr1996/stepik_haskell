module Section_3.Lesson_3.Step_3_3_4Spec where

import Section_3.Lesson_3.Step_3_3_4

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import Test.Hspec

strings :: [String]
strings = ["abc", "defg", "hij"]

spec :: Spec
spec = parallel $ do
  describe "logFirstAndRetSecond" $ do
    it "runReader (runWriterT logFirstAndRetSecond) strings" $ do
      runReader (runWriterT logFirstAndRetSecond) strings `shouldBe` ("DEFG", "abc")

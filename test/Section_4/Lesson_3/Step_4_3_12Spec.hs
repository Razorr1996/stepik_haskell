module Section_4.Lesson_3.Step_4_3_12Spec where

import Control.Monad.Trans.Except (Except, runExcept)
import Section_3.Lesson_1.Step_3_1_8 (ReadError (..))
import Section_4.Lesson_3.Step_4_3_12
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "ExceptT: tryRead" $ do
    it "runExcept (tryRead \"5\" :: Except ReadError Int)" $ do
      runExcept (tryRead "5" :: Except ReadError Int) `shouldBe` Right 5
    it "runExcept (tryRead \"5\" :: Except ReadError Double)" $ do
      runExcept (tryRead "5" :: Except ReadError Double) `shouldBe` Right 5.0
    it "runExcept (tryRead \"5zzz\" :: Except ReadError Int)" $ do
      runExcept (tryRead "5zzz" :: Except ReadError Int) `shouldBe` Left (NoParse "5zzz")
    it "runExcept (tryRead \"(True, ())\" :: Except ReadError (Bool, ()))" $ do
      runExcept (tryRead "(True, ())" :: Except ReadError (Bool, ())) `shouldBe` Right (True, ())
    it "runExcept (tryRead \"\" :: Except ReadError (Bool, ()))" $ do
      runExcept (tryRead "" :: Except ReadError (Bool, ())) `shouldBe` Left EmptyInput
    it "runExcept (tryRead \"wrong\" :: Except ReadError (Bool, ()))" $ do
      runExcept (tryRead "wrong" :: Except ReadError (Bool, ())) `shouldBe` Left (NoParse "wrong")

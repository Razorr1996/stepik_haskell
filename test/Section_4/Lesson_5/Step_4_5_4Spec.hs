module Section_4.Lesson_5.Step_4_5_4Spec where

import Control.Monad.State
import Section_4.Lesson_5.Step_4_5_4
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "MonadError: limited" $ do
    it "runLimited1 (< 3) [modify (+ 1), modify (+ 1), modify (+ 1), modify (+ 1)] 0" $
      do
        runLimited1 (< 3) [modify (+ 1), modify (+ 1), modify (+ 1), modify (+ 1)] 0
        `shouldBe` (Left 2, 3)

    it "runLimited2 (< 3) [modify (+ 1), modify (+ 1), modify (+ 1), modify (+ 1)] 0" $
      do
        runLimited2 (< 3) [modify (+ 1), modify (+ 1), modify (+ 1), modify (+ 1)] 0
        `shouldBe` Left 2

    it "runLimited1 (< 100) [modify (+ 1), modify (+ 1), modify (+ 1), modify (+ 1)] 0" $
      do
        runLimited1 (< 100) [modify (+ 1), modify (+ 1), modify (+ 1), modify (+ 1)] 0
        `shouldBe` (Right [(), (), (), ()], 4)

    it "runLimited2 (< 100) [modify (+ 1), modify (+ 1), modify (+ 1), modify (+ 1)] 0" $
      do
        runLimited2 (< 100) [modify (+ 1), modify (+ 1), modify (+ 1), modify (+ 1)] 0
        `shouldBe` Right ([(), (), (), ()], 4)

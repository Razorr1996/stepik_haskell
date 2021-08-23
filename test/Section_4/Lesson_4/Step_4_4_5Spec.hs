module Section_4.Lesson_4.Step_4_4_5Spec where

import Section_4.Lesson_4.Step_4_4_5
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "Functor': FunctionalDependencies, FlexibleInstances" $ do
    it "fmap' succ \"ABC\"" $
      do
        fmap' succ "ABC"
        `shouldBe` "BCD"

    it "fmap' (^ 2) (Just 42)" $
      do
        fmap' (^ 2) (Just 42)
        `shouldBe` Just 1764

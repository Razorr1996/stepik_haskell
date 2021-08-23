module Section_3.Lesson_1.Step_3_1_13Spec where

import Control.Monad.Trans.Except (Except, runExcept, withExcept)
import Data.Foldable (msum)

import Test.Hspec

import Section_3.Lesson_1.Step_3_1_7
import Section_3.Lesson_1.Step_3_1_13

toSimple :: Except ListIndexError a -> Either SimpleError a
toSimple = runExcept . withExcept lie2se

toSimpleFromList :: [Except ListIndexError a] -> Either SimpleError a
toSimpleFromList = runExcept . msum . map (withExcept lie2se)

xs :: [Integer]
xs = [1, 2, 3]

spec :: Spec
spec = parallel $ do
  describe "lie2se" $ do
    it "toSimple $ xs !!! 42" $
      do
        toSimple $ xs !!! 42
        `shouldBe` Left (Simple {getSimple = "[index (42) is too large]"})
    
    it "toSimple $ xs !!! (-2)" $
          do
            toSimple $ xs !!! (-2)
            `shouldBe` Left (Simple {getSimple = "[negative index]"})
    
    it "totoSimple $ xs !!! 2" $
          do
            toSimple $ xs !!! 2
            `shouldBe` Right 3
    
    it "toSimpleFromList [xs !!! (-2), xs !!! 42]" $
          do
            toSimpleFromList [xs !!! (-2), xs !!! 42]
            `shouldBe` Left (Simple {getSimple = "[negative index][index (42) is too large]"})

    it "toSimpleFromList [xs !!! (-2), xs !!! 2]" $
          do
            toSimpleFromList [xs !!! (-2), xs !!! 2]
            `shouldBe` Right 3

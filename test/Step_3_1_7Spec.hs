module Step_3_1_7Spec where

import Control.Monad.Trans.Except (runExcept)
import Step_3_1_7
import Test.Hspec

(!!!!) :: [a] -> Int -> Either ListIndexError a
(!!!!) xs n = runExcept $ xs !!! n

spec :: Spec
spec = do
  describe "!!!" $ do
    it "[1..100] !!! 5" $ do
      [1 .. 100] !!!! 5 `shouldBe` Right 6

    it "[1,2,3] !!! 0" $ do
      [1, 2, 3] !!!! 0 `shouldBe` Right 1

    it "[1,2,3] !!! 42" $ do
      [1, 2, 3] !!!! 42 `shouldBe` Left (ErrIndexTooLarge 42)

    it "[1,2,3] !!! (-3)" $ do
      [1, 2, 3] !!!! (-3) `shouldBe` Left ErrNegativeIndex

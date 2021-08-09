module Step_3_3_5Spec where

import Control.Monad.Trans.Writer
import Step_3_3_5
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "separate" $ do
    it "(runWriter . runWriterT) $ separate (< 3) (> 7) [0..10]" $
      do
        (runWriter . runWriterT) $ separate (< 3) (> 7) [0 .. 10]
        `shouldBe` ((([3, 4, 5, 6, 7], [0, 1, 2]), [8, 9, 10]) :: (([Int], [Int]), [Int]))

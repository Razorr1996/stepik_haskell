module Step_4_2_14Spec where

import Control.Monad.State
import Control.Monad.Writer
import Step_4_2_14
import Test.Hspec

numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)

spec :: Spec
spec = parallel $ do
  describe "StateT: Tree -> numberAndCount -> go" $ do
    it "numberAndCount (Leaf ())" $
      do
        numberAndCount (Leaf ())
        `shouldBe` (Leaf 1, 1)

    it "numberAndCount (Fork (Leaf ()) () (Leaf ()))" $
      do
        numberAndCount (Fork (Leaf ()) () (Leaf ()))
        `shouldBe` (Fork (Leaf 1) 2 (Leaf 3), 2)

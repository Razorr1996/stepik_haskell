module Step_4_2_11Spec where

import Control.Monad.IO.Class (liftIO)
import Step_4_2_11
import Test.Hspec

sl2 = StateT $ \st -> [(st, st), (st + 1, st -1)]

sm = StateT $ \st -> Just (st + 1, st -1)

spec :: Spec
spec = parallel $ do
  describe "StateT: fail to m" $ do
    it "runStateT (do 6 <- sl2; return ()) 5" $
      do
        runStateT (do 6 <- sl2; return ()) 5
        `shouldBe` ([((), 4)] :: [((), Integer)])

    it "runStateT (do 42 <- sm; return ()) 5" $
      do
        runStateT (do 42 <- sm; return ()) 5
        `shouldBe` (Nothing :: Maybe ((), Integer))

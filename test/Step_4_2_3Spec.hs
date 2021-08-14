module Step_4_2_3Spec where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.State (StateT (..), evalStateT)
import Step_4_2_3
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "StateT: readerToStateT" $ do
    it "evalStateT (readerToStateT $ asks (+ 2)) 4" $
      do
        x <- liftIO $ evalStateT (readerToStateT $ asks (+ 2)) 4
        x `shouldBe` (6 :: Integer)

    it "runStateT (readerToStateT $ asks (+ 2)) 4" $
      do
        x <- liftIO $ runStateT (readerToStateT $ asks (+ 2)) 4
        x `shouldBe` ((6, 4) :: (Integer, Integer))

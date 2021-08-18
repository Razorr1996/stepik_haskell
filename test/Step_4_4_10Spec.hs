module Step_4_4_10Spec where

import Control.Monad.State
import Step_4_4_10
import Test.Hspec

logSt' :: LoggT (State Integer) Integer
logSt' = do
  modify (+ 1) -- no lift!
  a <- get -- no lift!
  write2log $ show $ a * 10
  put 42 -- no lift!
  return $ a * 100

spec :: Spec
spec = parallel $ do
  describe "LoggT: MonadState" $ do
    it "runState (runLoggT logSt') 2" $
      do
        runState (runLoggT logSt') 2
        `shouldBe` (Logged "30" 300, 42)

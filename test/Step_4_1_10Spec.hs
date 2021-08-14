module Step_4_1_10Spec where

import Control.Monad.State
import Step_4_1_10
import Step_4_1_7
import Test.Hspec

logTst' :: Logg Integer
logTst' = do
  write2log "AAA"
  write2log "BBB"
  return 42

stLog :: StateT Integer Logg Integer
stLog = do
  modify (+ 1)
  a <- get
  lift $ write2log $ show $ a * 10
  put 42
  return $ a * 100

spec :: Spec
spec = parallel $ do
  describe "LoggT: write2log" $ do
    it "runLogg logTst'" $
      do
        runLogg logTst'
        `shouldBe` Logged "AAABBB" 42

    it "runLogg $ runStateT stLog 2" $
      do
        runLogg $ runStateT stLog 2
        `shouldBe` Logged "30" (300, 42)

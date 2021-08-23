module Section_4.Lesson_4.Step_4_4_12Spec where

import Control.Monad.Reader
import Control.Monad.State
import Section_4.Lesson_4.Step_4_4_11
import Section_4.Lesson_4.Step_4_4_12
import Test.Hspec

logSt'' :: LoggT (State Integer) Integer
logSt'' = do
  x <- logg $ Logged "BEGIN " 1
  modify (+ x)
  a <- get
  w2log $ show $ a * 10
  put 42
  w2log " END"
  return $ a * 100

rdrStLog :: ReaderT Integer (StateT Integer Logg) Integer
rdrStLog = do
  x <- logg $ Logged "BEGIN " 1
  y <- ask
  modify (+ (x + y))
  a <- get
  w2log $ show $ a * 10
  put 42
  w2log " END"
  return $ a * 100

spec :: Spec
spec = parallel $ do
  describe "LoggT: MonadLogg" $ do
    it "runState (runLoggT logSt'') 2" $
      do
        runState (runLoggT logSt'') 2
        `shouldBe` (Logged "BEGIN 30 END" 300, 42)

    it "runLogg $ runStateT (runReaderT rdrStLog 4) 2" $
      do
        runLogg $ runStateT (runReaderT rdrStLog 4) 2
        `shouldBe` Logged "BEGIN 70 END" (700, 42)

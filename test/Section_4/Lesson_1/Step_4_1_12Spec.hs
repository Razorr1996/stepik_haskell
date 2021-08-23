module Section_4.Lesson_1.Step_4_1_12Spec where

import Control.Monad.State
import Section_4.Lesson_1.Step_4_1_10
import Section_4.Lesson_1.Step_4_1_12 ()
import Section_4.Lesson_1.Step_4_1_7
import Test.Hspec

logSt :: LoggT (State Integer) Integer
logSt = do
  lift $ modify (+ 1)
  a <- lift get
  write2log $ show $ a * 10
  lift $ put 42
  return $ a * 100

spec :: Spec
spec = parallel $ do
  describe "LoggT: MonadTrans" $ do
    it "runState (runLoggT logSt) 2" $
      do
        runState (runLoggT logSt) 2
        `shouldBe` (Logged "30" 300, 42)

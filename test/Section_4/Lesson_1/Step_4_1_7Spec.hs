module Section_4.Lesson_1.Step_4_1_7Spec where

import Control.Monad.Identity
import Section_4.Lesson_1.Step_4_1_7
import Test.Hspec

logTst :: LoggT Identity Integer
logTst = do
  x <- LoggT $ Identity $ Logged "AAA" 30
  y <- return 10
  z <- LoggT $ Identity $ Logged "BBB" 2
  return $ x + y + z

failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
  5 <- LoggT $ fmap (Logged "") xs
  LoggT [Logged "A" ()]
  return 42

spec :: Spec
spec = parallel $ do
  describe "LoggT: Functor, Applicative, Monad" $ do
    it "runIdentity (runLoggT logTst)" $
      do
        runIdentity (runLoggT logTst)
        `shouldBe` Logged "AAABBB" 42

    it "runLoggT $ failTst [5,5]" $
      do
        runLoggT $ failTst [5, 5]
        `shouldBe` [Logged "A" 42, Logged "A" 42]

    it "runLoggT $ failTst [5,6]" $
      do
        runLoggT $ failTst [5, 6]
        `shouldBe` [Logged "A" 42]

    it "runLoggT $ failTst [7,6]" $
      do
        runLoggT $ failTst [7, 6]
        `shouldBe` []

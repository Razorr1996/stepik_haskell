{-# OPTIONS_GHC -Wno-x-partial #-}
module Section_3.Lesson_3.Step_3_3_7Spec where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Char
import Section_3.Lesson_3.Step_3_3_7
import Test.Hspec

logFirstAndRetSecond :: MyRW String
logFirstAndRetSecond = do
  el1 <- myAsks head
  el2 <- myAsks (map toUpper . head . tail)
  myTell el1
  return el2

strings :: [String]
strings = ["abc", "defg", "hij"]

spec :: Spec
spec = parallel $ do
  describe "logFirstAndRetSecond" $ do
    it "runWriter (runReaderT logFirstAndRetSecond strings)" $ do
      runWriter (runReaderT logFirstAndRetSecond strings) `shouldBe` ("DEFG", "abc")

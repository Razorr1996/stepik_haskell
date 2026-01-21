{-# OPTIONS_GHC -Wno-x-partial #-}
module Section_3.Lesson_3.Step_3_3_9Spec where

import Control.Monad.IO.Class (liftIO)
import Data.Char
import Section_3.Lesson_3.Step_3_3_9
import Test.Hspec

logFirstAndRetSecond :: MyRWT IO String
logFirstAndRetSecond = do
  el1 <- myAsks head
  myLift $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks (map toUpper . head . tail)
  myLift $ putStrLn $ "Second is " ++ show el2
  myTell el1
  return el2

strings :: [String]
strings = ["abc", "defg", "hij"]

spec :: Spec
spec = parallel $ do
  describe "logFirstAndRetSecond" $ do
    it "runMyRWT logFirstAndRetSecond strings" $
      do
        x <- liftIO $ runMyRWT logFirstAndRetSecond strings
        x `shouldBe` ("DEFG", "abc")

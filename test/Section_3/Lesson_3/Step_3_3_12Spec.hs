module Section_3.Lesson_3.Step_3_3_12Spec where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Section_3.Lesson_3.Step_3_3_12
import Test.Hspec

tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
--  lift $ print res
  put res
  return n

spec :: Spec
spec = parallel $ do
  describe "RiiEsSiT" $ do
    it "runRiiEsSiT (forever $ go tickCollatz') (1,200) 27" $
      do
        x <- liftIO $ runRiiEsSiT (forever $ go tickCollatz') (1, 200) 27
        x `shouldBe` ((Left "Upper bound", 214) :: (Either String (), Integer))

  describe "RiiEsSiT from EsSi" $ do
    it "runRiiEsSiT (go tickCollatz') (1, 85) 27" $
      do
        x <- liftIO $ runRiiEsSiT (go tickCollatz') (1, 85) 27
        x `shouldBe` ((Right (), 82) :: (Either String (), Integer))

    it "runRiiEsSiT (go tickCollatz') (1, 80) 27" $
      do
        x <- liftIO $ runRiiEsSiT (go tickCollatz') (1, 80) 27
        x `shouldBe` ((Left "Upper bound", 82) :: (Either String (), Integer))

    it "runRiiEsSiT (forever $ go tickCollatz') (1, 1000) 27" $
      do
        x <- liftIO $ runRiiEsSiT (forever $ go tickCollatz') (1, 1000) 27
        x `shouldBe` ((Left "Upper bound", 1186) :: (Either String (), Integer))

    it "runRiiEsSiT (forever $ go tickCollatz') (1, 10000) 27" $
      do
        x <- liftIO $ runRiiEsSiT (forever $ go tickCollatz') (1, 10000) 27
        x `shouldBe` ((Left "Lower bound", 1) :: (Either String (), Integer))

module Step_3_4_13Spec where

import Step_3_4_13
import Test.Hspec

a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)

spec :: Spec
spec = parallel $ do
  describe "Arr2T & Arr3T: MonadFail" $ do
    it "getArr3T (do {9 <- a3m; y <- a3m; return y}) 2 3 4" $
      do
        getArr3T (do 9 <- a3m; y <- a3m; return y) 2 3 4
        `shouldBe` (Just 9 :: Maybe Integer)

    it "getArr3T (do {10 <- a3m; y <- a3m; return y}) 2 3 4" $
      do
        getArr3T (do 10 <- a3m; y <- a3m; return y) 2 3 4
        `shouldBe` (Nothing :: Maybe Integer)

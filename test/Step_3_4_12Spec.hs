module Step_3_4_12Spec where

import Step_3_4_12
import Test.Hspec

a2l = Arr2T $ \e1 e2 -> [e1, e2]

a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)

spec :: Spec
spec = parallel $ do
  describe "Arr2T & Arr3T: Monad" $ do
    it "getArr2T (do {x <- a2l; y <- a2l; return (x + y)}) 3 5" $
      do
        getArr2T (do x <- a2l; y <- a2l; return (x + y)) 3 5
        `shouldBe` ([6, 8, 8, 10] :: [Integer])

    it "getArr3T (do {x <- a3m; y <- a3m; return (x * y)}) 2 3 4" $
      do
        getArr3T (do x <- a3m; y <- a3m; return (x * y)) 2 3 4
        `shouldBe` (Just 81 :: Maybe Integer)

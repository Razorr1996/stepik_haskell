module Step_3_4_16Spec where

import Step_3_4_16
import Control.Monad.Trans
import Test.Hspec

a2l = Arr2T $ \e1 e2 -> [e1, e2]

spec :: Spec
spec = parallel $ do
  describe "Arr2T & Arr3T: MonadTrans" $ do
    it "getArr2T (do {x <- a2l; y <- lift [10,20,30]; return (x+y)}) 3 4" $
      do
        getArr2T (do x <- a2l; y <- lift [10, 20, 30]; return (x + y)) 3 4
        `shouldBe` ([13, 23, 33, 14, 24, 34] :: [Integer])

    it "getArr2T (do {x <- asks2 const; y <- asks2 (flip const); z <- asks2 (,); return (x,y,z)}) 'A' 'B'" $
      do
        x <- liftIO $ getArr2T (do {x <- asks2 const; y <- asks2 (flip const); z <- asks2 (,); return (x,y,z)}) 'A' 'B'
        x `shouldBe` ('A', 'B', ('A', 'B'))

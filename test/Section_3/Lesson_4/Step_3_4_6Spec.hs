module Section_3.Lesson_4.Step_3_4_6Spec where

import Section_3.Lesson_4.Step_3_4_6
import Test.Hspec

a2l = Arr2T $ \e1 e2 -> [e1, e2, e1 + e2]

a3e = Arr3T $ \e1 e2 e3 -> Right (e1 + e2 + e3)

spec :: Spec
spec = parallel $ do
  describe "Arr2T & Arr3T: Functor" $ do
    it "(getArr2T $ succ <$> a2l) 10 100" $
      do
        (getArr2T $ succ <$> a2l) 10 100
        `shouldBe` ([11, 101, 111] :: [Integer])

    it "(getArr3T $ sqrt <$> a3e) 2 3 4" $
      do
        (getArr3T $ sqrt <$> a3e) 2 3 4
        `shouldBe` (Right 3.0 :: Either () Double)

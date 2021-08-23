module Section_3.Lesson_4.Step_3_4_10Spec where

import Section_3.Lesson_4.Step_3_4_10
import Test.Hspec

a2l = Arr2T $ \e1 e2 -> [e1, e2]
a2fl = Arr2T $ \e1 e2 -> [(e1 * e2 +), const 7]

a3l = Arr3T $ \e1 e2 e3 -> [e1, e2]
a3fl = Arr3T $ \e1 e2 e3 -> [(e2 +), (e3 +)]

spec :: Spec
spec = parallel $ do
  describe "Arr2T & Arr3T: Applicative" $ do
    it "getArr2T (a2fl <*> a2l) 2 10" $
      do
        getArr2T (a2fl <*> a2l) 2 10
        `shouldBe` ([22, 30, 7, 7] :: [Integer])

    it "getArr3T (a3fl <*> a3l) 3 5 7" $
      do
        getArr3T (a3fl <*> a3l) 3 5 7
        `shouldBe` ([8, 10, 10, 12] :: [Integer])

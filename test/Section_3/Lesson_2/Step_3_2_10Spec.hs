module Section_3.Lesson_2.Step_3_2_10Spec where

import Section_3.Lesson_1.Step_3_1_8
import Section_3.Lesson_2.Step_3_2_10
import Test.Hspec

add :: Int -> Int -> FailCont r e Int
add x y = FailCont $ \ok _ -> ok $ x + y

addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2

spec :: Spec
spec = parallel $ do
  describe "FailCont" $ do
    it "evalFailCont $ addInts \"15\" \"12\"" $
      do
        evalFailCont $ addInts "15" "12"
        `shouldBe` (Right 27 :: Either a Int)

    it "runFailCont (addInts \"15\" \"\") show ((\"Oops: \" ++) . show)" $
      do
        runFailCont (addInts "15" "") show (("Oops: " ++) . show)
        `shouldBe` "Oops: EmptyInput"

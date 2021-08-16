module Step_4_3_6Spec where

import Step_4_3_6
import Test.Hspec

map1 :: GameMap
map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise = Chasm

spec :: Spec
spec = parallel $ do
  describe "ExceptT: waysToDie" $ do
    it "waysToDie Poisoned map1 1 (4, 2)" $
      do
        waysToDie Poisoned map1 1 (4, 2)
        `shouldBe` 1

    it "waysToDie Poisoned map1 2 (4, 2)" $
      do
        waysToDie Poisoned map1 2 (4, 2)
        `shouldBe` 2

    it "waysToDie Poisoned map1 3 (4, 2)" $
      do
        waysToDie Poisoned map1 3 (4, 2)
        `shouldBe` 5

    it "waysToDie Poisoned map1 4 (4, 2)" $
      do
        waysToDie Poisoned map1 4 (4, 2)
        `shouldBe` 13

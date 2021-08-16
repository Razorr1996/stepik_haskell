module Step_4_3_6 where

import qualified Control.Monad.Except as E

data Tile = Floor | Chasm | Snake
  deriving (Show)

data DeathReason = Fallen | Poisoned
  deriving (Eq, Show)

type Point = (Integer, Integer)

type GameMap = Point -> Tile

-- region Task
-- code
tileResult :: GameMap -> Point -> Either DeathReason Point
tileResult g p = case g p of
  Chasm -> Left Fallen
  Snake -> Left Poisoned
  _ -> Right p

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves g 0 p = [tileResult g p]
moves g n p@(x, y) = E.runExceptT h
  where
    h :: E.ExceptT DeathReason [] Point
    h = do
      _ <- E.liftEither $ tileResult g p
      p' <- E.lift [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
      e <- E.lift $ moves g (n - 1) p'
      E.liftEither e

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie d g n p = length $ filter (\x -> Left d == x) $ moves g n p

-- endregion

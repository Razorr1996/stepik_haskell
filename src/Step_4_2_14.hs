module Step_4_2_14 where

import Control.Monad.State
import Control.Monad.Writer

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
  deriving (Eq, Show)

-- region Task
-- code
go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
go (Leaf _) = do
  lift $ tell $ Sum 1
  n <- get
  modify (+ 1)
  return $ Leaf n
go (Fork l _ r) = do
  l' <- go l
  n <- get
  modify (+ 1)
  r' <- go r
  return $ Fork l' n r'

-- endregion

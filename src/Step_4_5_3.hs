module Step_4_5_3 where

import Step_4_5_2

-- region Task
-- code
import Control.Monad.Except
import Control.Monad.Writer
import Data.Foldable
import Data.Traversable

treeSum :: Tree String -> Either ReadError Integer
treeSum t = helper $ runWriter $ runExceptT $ traverse_ go t
  where
    helper (Left e, _) = Left e
    helper (_, s) = Right $ getSum s

    go :: String -> ExceptT ReadError (Writer (Sum Integer)) ()
    go s = do
      val <- tryRead s
      tell $ Sum val

-- endregion

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

instance Functor Tree where
  fmap = fmapDefault

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Fork l k r) = Fork <$> traverse f l <*> f k <*> traverse f r

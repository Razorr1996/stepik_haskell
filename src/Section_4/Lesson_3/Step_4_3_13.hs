module Section_4.Lesson_3.Step_4_3_13 where

import Control.Monad.Except
import Control.Monad.Writer
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Section_3.Lesson_1.Step_3_1_8 (ReadError (..))
import Section_4.Lesson_3.Step_4_3_12

-- region Task
go :: String -> ExceptT ReadError (Writer (Sum Integer)) ()
go s = do
  val <- tryRead s
  lift $ tell $ Sum val

-- endregion

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

instance Functor Tree where
  fmap = fmapDefault

instance Foldable Tree where
  foldMap f (Leaf a) = f a
  foldMap f (Fork l x r) = foldMap f l <> f x <> foldMap f r

instance Traversable Tree where
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Fork l k r) = Fork <$> traverse f l <*> f k <*> traverse f r

treeSum :: Tree String -> (Maybe ReadError, Integer)
treeSum t =
  let (err, s) = runWriter . runExceptT $ traverse_ go t
   in (maybeErr err, getSum s)
  where
    maybeErr :: Either ReadError () -> Maybe ReadError
    maybeErr = either Just (const Nothing)

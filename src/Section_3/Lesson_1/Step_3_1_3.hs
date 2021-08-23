module Section_3.Lesson_1.Step_3_1_3 where

newtype Except e a = Except {runExcept :: Either e a} deriving (Show)

instance Functor (Except e) where
  fmap f (Except x) = Except (fmap f x)

instance Applicative (Except e) where
  pure x = Except (Right x)
  (Except f) <*> (Except x) = Except (f <*> x)

instance Monad (Except e) where
  m >>= k = case runExcept m of
    Left l -> Except $ Left l
    Right r -> k r

except :: Either e a -> Except e a
except = Except

-- region Task
withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f x = case runExcept x of
  Left s -> Except $ Left (f s)
  Right v -> Except $ Right v
-- endregion

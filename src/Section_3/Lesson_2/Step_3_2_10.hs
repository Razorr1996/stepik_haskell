module Section_3.Lesson_2.Step_3_2_10 where

-- region Task
-- code
import Control.Monad
import Control.Monad.Trans.Except (Except, runExcept)

newtype FailCont r e a = FailCont {runFailCont :: (a -> r) -> (e -> r) -> r}

instance Functor (FailCont r e) where
  fmap f m = FailCont $ \ok err -> runFailCont m (ok . f) err

instance Applicative (FailCont r e) where
--  pure = return
  pure x = FailCont $ \ok _ -> ok x
  (<*>) = ap

instance Monad (FailCont r e) where
--  return x = FailCont $ \ok _ -> ok x
  (FailCont m) >>= k = FailCont $ \ok err -> m (\a -> runFailCont (k a) ok err) err

toFailCont :: Except e a -> FailCont r e a
toFailCont e = FailCont $ \ok err -> either err ok $ runExcept e

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont m = runFailCont m Right Left

-- endregion

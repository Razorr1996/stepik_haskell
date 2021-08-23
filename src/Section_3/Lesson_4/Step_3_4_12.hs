{-# LANGUAGE InstanceSigs #-}
module Section_3.Lesson_4.Step_3_4_12 where

-- region Task
-- code
{-# LANGUAGE InstanceSigs #-}

newtype Arr2T e1 e2 m a = Arr2T {getArr2T :: e1 -> e2 -> m a}
newtype Arr3T e1 e2 e3 m a = Arr3T {getArr3T :: e1 -> e2 -> e3 -> m a}

instance Functor m => Functor (Arr2T e1 e2 m) where
  fmap :: (a -> b) -> Arr2T e1 e2 m a -> Arr2T e1 e2 m b
  fmap f r = Arr2T $ \e1 e2 -> f <$> getArr2T r e1 e2

instance Functor m => Functor (Arr3T e1 e2 e3 m) where
  fmap :: (a -> b) -> Arr3T e1 e2 e3 m a -> Arr3T e1 e2 e3 m b
  fmap f r = Arr3T $ \e1 e2 e3 -> f <$> getArr3T r e1 e2 e3

instance Applicative m => Applicative (Arr2T e1 e2 m) where
  pure :: a -> Arr2T e1 e2 m a
  pure x = Arr2T $ \_ _ -> pure x

  (<*>) :: Arr2T e1 e2 m (a -> b) -> Arr2T e1 e2 m a -> Arr2T e1 e2 m b
  f <*> v = Arr2T $ \e1 e2 -> getArr2T f e1 e2 <*> getArr2T v e1 e2

instance Applicative m => Applicative (Arr3T e1 e2 e3 m) where
  pure :: a -> Arr3T e1 e2 e3 m a
  pure x = Arr3T $ \_ _ _ -> pure x

  (<*>) :: Arr3T e1 e2 e3 m (a -> b) -> Arr3T e1 e2 e3 m a -> Arr3T e1 e2 e3 m b
  f <*> v = Arr3T $ \e1 e2 e3 -> getArr3T f e1 e2 e3 <*> getArr3T v e1 e2 e3

instance Monad m => Monad (Arr2T e1 e2 m) where
  return :: a -> Arr2T e1 e2 m a
  return = pure

  (>>=) :: Arr2T e1 e2 m a -> (a -> Arr2T e1 e2 m b) -> Arr2T e1 e2 m b
  m >>= k = Arr2T $ \e1 e2 -> do
    v <- getArr2T m e1 e2
    getArr2T (k v) e1 e2

instance Monad m => Monad (Arr3T e1 e2 e3 m) where
  return :: a -> Arr3T e1 e2 e3 m a
  return = pure

  (>>=) :: Arr3T e1 e2 e3 m a -> (a -> Arr3T e1 e2 e3 m b) -> Arr3T e1 e2 e3 m b
  m >>= k = Arr3T $ \e1 e2 e3 -> do
    v <- getArr3T m e1 e2 e3
    getArr3T (k v) e1 e2 e3

-- endregion

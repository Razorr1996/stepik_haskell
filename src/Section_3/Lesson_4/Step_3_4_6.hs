{-# LANGUAGE InstanceSigs #-}
module Section_3.Lesson_4.Step_3_4_6 where

-- region Task
-- code
newtype Arr2T e1 e2 m a = Arr2T {getArr2T :: e1 -> e2 -> m a}
newtype Arr3T e1 e2 e3 m a = Arr3T {getArr3T :: e1 -> e2 -> e3 -> m a}

instance Functor m => Functor (Arr2T e1 e2 m) where
  fmap :: (a -> b) -> Arr2T e1 e2 m a -> Arr2T e1 e2 m b
  fmap f r = Arr2T $ \e1 e2 -> f <$> getArr2T r e1 e2

instance Functor m => Functor (Arr3T e1 e2 e3 m) where
  fmap :: (a -> b) -> Arr3T e1 e2 e3 m a -> Arr3T e1 e2 e3 m b
  fmap f r = Arr3T $ \e1 e2 e3 -> f <$> getArr3T r e1 e2 e3

-- endregion

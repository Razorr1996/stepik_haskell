module Section_4.Lesson_1.Step_4_1_7 where

import Control.Applicative (liftA2)

data Logged a = Logged String a deriving (Eq, Show)

newtype LoggT m a = LoggT {runLoggT :: m (Logged a)}

-- region Task
-- code
instance Functor m => Functor (LoggT m) where
  fmap f = LoggT . fmap updater . runLoggT
    where
      updater (Logged l a) = Logged l (f a)

instance Applicative m => Applicative (LoggT m) where
  pure x = LoggT $ pure $ Logged mempty x

  f <*> v = LoggT $ liftA2 updater (runLoggT f) (runLoggT v)
    where
      updater (Logged l1 f') (Logged l2 v') = Logged (l1 `mappend` l2) (f' v')

instance Monad m => Monad (LoggT m) where
  return = pure

  m >>= k = LoggT $ do
    ~(Logged l1 v) <- runLoggT m
    ~(Logged l2 v') <- runLoggT (k v)
    return $ Logged (l1 `mappend` l2) v'

instance MonadFail m => MonadFail (LoggT m) where
  fail = LoggT . fail

-- endregion

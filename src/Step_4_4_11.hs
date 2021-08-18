{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Step_4_4_11 where

-- region Task
-- code
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative (liftA2)
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Identity

-- endregion

type Logg = LoggT Identity

data Logged a = Logged String a deriving (Eq, Show)

newtype LoggT m a = LoggT {runLoggT :: m (Logged a)}

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

instance MonadTrans LoggT where
  lift m = LoggT $ Logged mempty <$> m

instance MonadState s m => MonadState s (LoggT m) where
  get   = lift get
  put   = lift . put
  state = lift . state

-- region Task
-- code
mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f = LoggT . f . runLoggT

instance MonadReader r m => MonadReader r (LoggT m) where
  ask    = lift ask
  local  = mapLoggT . local
  reader = lift . reader

-- endregion

write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return $ Logged s ()

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

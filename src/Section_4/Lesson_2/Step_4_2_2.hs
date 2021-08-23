module Section_4.Lesson_2.Step_4_2_2 where

import Control.Monad.State (StateT (..))

-- region Task
-- code
evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m s = do
  ~(a, _) <- runStateT m s
  return a

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m s = do
  ~(_, s') <- runStateT m s
  return s'

-- endregion

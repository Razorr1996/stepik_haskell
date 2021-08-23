{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Section_4.Lesson_4.Step_4_4_12 where

import Section_4.Lesson_4.Step_4_4_11

-- region Task
-- code
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Reader

class Monad m => MonadLogg m where
  w2log :: String -> m ()
  logg  :: Logged a -> m a

instance Monad m => MonadLogg (LoggT m) where
  w2log = write2log
  logg  = LoggT . pure

instance MonadLogg m => MonadLogg (StateT s m) where
  w2log = lift . w2log
  logg  = lift . logg

instance MonadLogg m => MonadLogg (ReaderT r m) where
  w2log = lift . w2log
  logg  = lift . logg

-- endregion

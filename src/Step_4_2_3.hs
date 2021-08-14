module Step_4_2_3 where

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))

-- region Task
-- code
readerToStateT :: Monad m => ReaderT r m a -> StateT r m a
readerToStateT r = StateT $ \s -> do
  x <- runReaderT r s
  return (x, s)

-- endregion

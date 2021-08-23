module Section_3.Lesson_3.Step_3_3_12 where

-- region Task
-- code
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

type RiiEsSiT m = ReaderT (Integer,Integer) (ExceptT String (StateT Integer m))

runRiiEsSiT :: ReaderT (Integer,Integer) (ExceptT String (StateT Integer m)) a
                 -> (Integer,Integer)
                 -> Integer
                 -> m (Either String a, Integer)
runRiiEsSiT riiessit r = runStateT (runExceptT (runReaderT riiessit r))

go :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go s = do
  (minB, maxB) <- ask
  lift $ do
    nextInE <- lift $ do
      cur <- get
      nextInS <- lift $ do
        execStateT s cur
      put nextInS
      return nextInS
    when (nextInE <= minB) $ throwE "Lower bound"
    when (nextInE >= maxB) $ throwE "Upper bound"
  return () -- unnecessary

-- endregion

module Step_3_3_9 where

-- region Task
-- code
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

type MyRWT m a = ReaderT [String] (WriterT String m) a

runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT rwt s = runWriterT (runReaderT rwt s)

myAsks :: Monad m => ([String] -> a) -> MyRWT m a
myAsks = asks

myTell :: Monad m => String -> MyRWT m ()
myTell = lift . tell

myLift :: Monad m => m a -> MyRWT m a
myLift = lift . lift

-- endregion

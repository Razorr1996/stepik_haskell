module Step_4_1_10 where

import Control.Monad.Identity
import Step_4_1_7

-- region Task
-- code
write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return $ Logged s ()

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

-- endregion

module Step_3_3_7 where

-- region Task
-- imports

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

-- endregion

type MyRW a = ReaderT [String] (Writer String) a

-- region Task
-- code
myAsks :: ([String] -> a) -> MyRW a
myAsks = asks

myTell :: String -> MyRW ()
myTell = lift . tell

-- endregion

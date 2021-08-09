module Step_3_3_11 where

-- region Task
-- code
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n

type EsSi = ExceptT String (State Integer)

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi essi = runState (runExceptT essi)

go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go minB maxB s = do
  cur <- lift get
  let next = execState s cur
  lift $ put next
  when (next <= minB) $ throwE "Lower bound"
  when (next >= maxB) $ throwE "Upper bound"

-- endregion

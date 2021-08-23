{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Section_4.Lesson_5.Step_4_5_4 where

-- region Task
-- code
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable

run1 :: ExceptT Int (State s) [a] -> s -> (Either Int [a], s)
run1 m = runState (runExceptT m)

run2 :: ExceptT Int (State s) [a] -> s -> Either Int ([a], s)
run2 m st = either Left (\a -> Right (a, s)) e
  where
    (e, s) = runState (runExceptT m) st

-- endregion

--limited :: (s -> Bool) -> [State s a] -> StateT s (Except Int) [a]
limited :: (MonadState s m, MonadError e m, Num e, Enum e) => (s -> Bool) -> [State s a] -> m [a]
limited p fs = traverse limit1 (zip [0 ..] fs)
  where
    --    limit1 :: (Int, State s a) ->
    limit1 (i, f) = do
      a <- state (runState f)
      stateIsBad <- gets (not . p)
      when stateIsBad $ throwError i
      pure a

runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)
runLimited1 p fs = run1 (limited p fs)

runLimited2 :: (s -> Bool) -> [State s a] -> s -> Either Int ([a], s)
runLimited2 p fs = run2 (limited p fs)

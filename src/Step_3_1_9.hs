module Step_3_1_9 where

import Control.Monad (zipWithM)
import Control.Monad.Trans.Except
import Step_3_1_8

data SumError = SumError Int ReadError
  deriving (Show, Eq)

-- region Task
trySum :: [String] -> Except SumError Integer
trySum xs = except $ sum <$> zipWithM f (runExcept . tryRead <$> xs) [1 ..]
  where
    f (Left e) n = Left $ SumError n e
    f (Right r) _ = Right r

-- endregion

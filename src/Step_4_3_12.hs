module Step_4_3_12 where

import Control.Monad.Trans.Except
import Step_3_1_8 (ReadError (..))

-- region Task
tryRead :: (Read a, Monad m) => String -> ExceptT ReadError m a
tryRead "" = throwE EmptyInput
tryRead s = do
  case reads s of
    [(x, "")] -> return x
    _ -> throwE $ NoParse s

-- endregion

module Step_3_1_8 where

import Control.Monad.Trans.Except

data ReadError = EmptyInput | NoParse String
  deriving (Show, Eq)

-- region Task
tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE EmptyInput
tryRead s = do
  case reads s of
    [(x, "")] -> return x
    _ -> throwE $ NoParse s
-- endregion

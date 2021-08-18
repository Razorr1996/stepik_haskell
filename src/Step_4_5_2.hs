{-# LANGUAGE FlexibleContexts #-}

module Step_4_5_2 where

-- region Task
-- code
--{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Except

tryRead :: (Read a, MonadError ReadError m) => String -> m a
tryRead "" = throwError EmptyInput
tryRead s = do
  case reads s of
    [(x, "")] -> return x
    _ -> throwError $ NoParse s

-- endregion

data ReadError = EmptyInput | NoParse String
  deriving (Eq, Show)

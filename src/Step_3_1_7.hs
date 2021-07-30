module Step_3_1_7 where

import Control.Monad.Trans.Except

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
  deriving (Eq, Show)

-- region Task
infixl 9 !!!

(!!!) :: [a] -> Int -> Except ListIndexError a
arr !!! n
  | n < 0 = except $ Left ErrNegativeIndex
  | otherwise = f arr n n
  where
    f [] _ startIndex = except $ Left $ ErrIndexTooLarge startIndex
    f (x : _) 0 _ = except $ Right x
    f (_ : xs) n1 startIndex = f xs (n1 - 1) startIndex
-- endregion

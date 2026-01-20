module Section_3.Lesson_1.Step_3_1_13 where

import Section_3.Lesson_1.Step_3_1_7 (ListIndexError (ErrIndexTooLarge, ErrNegativeIndex))

newtype SimpleError = Simple {getSimple :: String}
  deriving (Eq, Show)

instance Semigroup SimpleError where
--  (<>) = mappend
  (Simple e1) <> (Simple e2) = Simple $ e1 ++ e2

-- region Task
instance Monoid SimpleError where
  mempty = Simple ""
--  (Simple e1) `mappend` (Simple e2) = Simple $ e1 ++ e2

lie2se :: ListIndexError -> SimpleError
lie2se ErrNegativeIndex = Simple "[negative index]"
lie2se (ErrIndexTooLarge n) = Simple $ "[index (" ++ show n ++ ") is too large]"

-- endregion

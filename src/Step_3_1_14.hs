module Step_3_1_14 where

-- region Task
import Control.Monad.Trans.Except (Except, except, runExcept)
import Data.Foldable (fold)
import Data.Monoid (Sum (Sum, getSum))
-- endregion

import Step_3_1_8
import Step_3_1_9

newtype Validate e a = Validate {getValidate :: Either [e] a}

instance Monoid a => Semigroup (Validate e a) where
  (<>) = mappend

-- region Task
instance Monoid a => Monoid (Validate e a) where
  mempty = Validate $ Right mempty
  (Validate (Right r1)) `mappend` (Validate (Right r2)) = Validate $ Right $ r1 `mappend` r2
  (Validate (Left e1)) `mappend` (Validate (Left e2)) = Validate $ Left $ e1 ++ e2
  (Validate (Left e1)) `mappend` _ = Validate $ Left e1
  _ `mappend` (Validate (Left e2)) = Validate $ Left e2

instance Functor (Validate e) where
  fmap _ (Validate (Left e)) = Validate $ Left e
  fmap f (Validate (Right r)) = Validate $ Right $ f r

collectE :: Except e a -> Validate e a
collectE = Validate . either (\x -> Left [x]) Right . runExcept

validateSum :: [String] -> Validate SumError Integer
validateSum xs = (getSum <$>) $ fold $ collectE <$> zipWith f (runExcept . tryRead <$> xs) [1 ..]
  where
    f (Left e) n = except $ Left $ SumError n e
    f (Right r) _ = except $ Right $ Sum r

-- endregion

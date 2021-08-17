{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Step_4_4_5 where

-- region Task
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

class Functor' c e | c -> e where
  fmap' :: (e -> e) -> c -> c

instance Functor' (Maybe a) a where
  fmap' = fmap

instance Functor' [a] a where
  fmap' = fmap

-- endregion

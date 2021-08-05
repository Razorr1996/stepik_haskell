module Step_3_3_5 where

-- region Task
-- code

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer

separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate p1 p2 xs = do
  tell $ filter p1 xs
  lift $ tell $ filter p2 xs
  return $ filter (\x -> not (p1 x) && not (p2 x)) xs

-- endregion

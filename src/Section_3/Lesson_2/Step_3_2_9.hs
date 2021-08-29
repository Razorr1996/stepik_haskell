module Section_3.Lesson_2.Step_3_2_9 where

import Control.Monad.Cont
-- region Task
-- code
import Control.Monad.Identity

type Checkpointed a = (a -> Cont a a) -> Cont a a

runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed p c = runCont (c f) id
  where
    f x = cont $ \c' -> runIdentity $ do
      let x' = c' x
      let r = case (p x, p x') of
            (_, False) -> x
            (_, _) -> x'
      return r

-- endregion

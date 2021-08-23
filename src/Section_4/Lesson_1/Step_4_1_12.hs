module Section_4.Lesson_1.Step_4_1_12 where

import Control.Monad.Trans
import Section_4.Lesson_1.Step_4_1_7

-- region Task
-- code
instance MonadTrans LoggT where
  lift m = LoggT $ Logged mempty <$> m

-- endregion

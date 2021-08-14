module Step_4_1_12 where

import Control.Monad.Trans
import Step_4_1_7

-- region Task
-- code
instance MonadTrans LoggT where
  lift m = LoggT $ Logged mempty <$> m

-- endregion

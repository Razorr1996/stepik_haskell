module Step_3_5_12 where

import Control.Monad.Trans
import Step_3_5_7

-- region Task
-- code
instance MonadTrans LoggT where
  lift m = LoggT $ Logged mempty <$> m

-- endregion

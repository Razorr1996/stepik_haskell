module Section_3.Lesson_2.Step_3_2_6 where

-- region Task
-- imports

-- endregion

import Control.Monad.Trans.Cont

-- region Task
-- code
showCont :: Show a => Cont String a -> String
showCont c = runCont c show

-- endregion

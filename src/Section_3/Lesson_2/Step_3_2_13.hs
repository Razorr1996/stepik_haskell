module Section_3.Lesson_2.Step_3_2_13 where

import Section_3.Lesson_2.Step_3_2_10

-- region Task
-- code
callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
callCFC f = FailCont $ \ok err -> runFailCont (f $ \a -> FailCont $ \_ _ -> ok a) ok err

-- endregion

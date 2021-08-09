module Step_3_3_10 where

-- region Task
-- import
import Control.Monad.Trans.Reader (ask)
import Data.Char (toUpper)

-- endregion

import Step_3_3_9

myAsk :: Monad m => MyRWT m [String]
myAsk = ask

-- region Task
-- code
veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
  xs <- myAsk
  let evenList = filter (even . length) xs
  let oddList = filter (odd . length) xs
  case (evenList, oddList) of
    (el1 : el2 : _, ol1 : ol2 : _) -> do
      myTell $ el1 ++ "," ++ ol1
      return (map toUpper el2, map toUpper ol2)
    _ -> myLift Nothing

-- endregion

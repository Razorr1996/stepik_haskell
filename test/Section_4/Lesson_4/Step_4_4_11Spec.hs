module Section_4.Lesson_4.Step_4_4_11Spec where

import Control.Monad.Reader
import Section_4.Lesson_4.Step_4_4_11
import Test.Hspec

logRdr :: LoggT (Reader [(Int, String)]) ()
logRdr = do
  x <- asks $ lookup 2 -- no lift!
  write2log (maybe "Nothing" id x)
  y <- local ((3, "Jim") :) $ asks $ lookup 3 -- no lift!
  write2log (maybe "Nothing" id y)

spec :: Spec
spec = parallel $ do
  describe "LoggT: MonadReader" $ do
    it "runReader (runLoggT logRdr) [(1, \"John\"), (2, \"Jane\")]" $
      do
        runReader (runLoggT logRdr) [(1, "John"), (2, "Jane")]
        `shouldBe` Logged "JaneJim" ()

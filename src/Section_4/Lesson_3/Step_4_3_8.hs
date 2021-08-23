{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Section_4.Lesson_3.Step_4_3_8 where

-- region Task
-- code
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Char (isNumber, isPunctuation)
import Data.Foldable (msum)

--instance Monoid PwdError where
--  mempty = PwdError mempty
--  PwdError s1 `mappend` PwdError s2 = PwdError $ s1 `mappend` s2

getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
  s <- liftIO getLine
  case isValid s of
    Left e@(PwdError errString) -> do
      liftIO $ putStrLn errString
      throwE e
    Right r -> return r

isValid :: String -> Either PwdError String
isValid s
  | length s < 8 = Left $ PwdError "Incorrect input: password is too short!"
  | not (any isNumber s) = Left $ PwdError "Incorrect input: password must contain some digits!"
  | not (any isPunctuation s) = Left $ PwdError "Incorrect input: password must contain some punctuation!"
  | otherwise = Right s

-- endregion

newtype PwdError = PwdError String
  deriving (Eq, Show, Semigroup, Monoid)

type PwdErrorIOMonad = ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

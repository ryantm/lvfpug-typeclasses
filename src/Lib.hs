{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib (
    printTime
  , myTime
  , CurrentTime (..)) where

import qualified Data.Time.Clock
import Data.Time.Clock (UTCTime)
import Data.Time.Format

import Control.Monad.IO.Class

class Monad m => CurrentTime m where
  getCurrentTime :: m UTCTime

newtype AppM a = AppM { runAppM :: IO a }
 deriving (Functor, Applicative, Monad, MonadIO)

instance CurrentTime AppM where
  getCurrentTime = liftIO Data.Time.Clock.getCurrentTime

myTime :: CurrentTime m => m String
myTime = do
  now <- getCurrentTime
  return ("The time is " ++ (formatTime defaultTimeLocale rfc822DateFormat now))

printTime :: IO ()
printTime = do
  s <- runAppM myTime
  putStrLn s

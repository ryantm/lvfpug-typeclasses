{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import qualified Data.Time.Clock
import qualified Network.BSD
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import Data.Monoid

import Control.Monad.IO.Class

class Monad m => CurrentTime m where
  getCurrentTime :: m UTCTime

class Monad m => HostName m where
  getHostName :: m Network.BSD.HostName

newtype AppM a = AppM { runAppM :: IO a }
 deriving (Functor, Applicative, Monad, MonadIO)

instance CurrentTime AppM where
  getCurrentTime = liftIO Data.Time.Clock.getCurrentTime

instance HostName AppM where
  getHostName = liftIO Network.BSD.getHostName

myTime :: CurrentTime m => m String
myTime = do
  now <- getCurrentTime
  return ("The time is " ++ (formatTime defaultTimeLocale rfc822DateFormat now))

myHostName :: HostName m => m String
myHostName = do
  n <- getHostName
  return ("The hostname of this computer is " ++ show n)

both :: (HostName m, CurrentTime m) => m String
both = fmap mconcat (sequenceA [myTime, pure "\n", myHostName])

printTime :: IO ()
printTime = do
  s <- runAppM both
  putStrLn s

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Lib
import Data.Functor.Identity
import Data.Time.Clock.POSIX
import System.Exit

newtype TestM a = TestM (Identity a)
 deriving (Functor, Applicative, Monad)

runTestM :: TestM a -> a
runTestM (TestM x) = runIdentity x

instance CurrentTime TestM where
  getCurrentTime = return (posixSecondsToUTCTime 0)

main :: IO ()
main = do
  if runTestM myTime == "The time is Thu,  1 Jan 1970 00:00:00 UTC"
    then
    exitSuccess
    else
    exitFailure

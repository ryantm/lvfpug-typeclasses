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

instance HostName TestM where
  getHostName = return "myhostname"



main :: IO ()
main = do
  if runTestM myTime /= "The time is Thu,  1 Jan 1970 00:00:00 UTC"
    then
    exitFailure
    else
    if runTestM myHostName /=
       "The hostname of this computer is \"myhostname\""
    then
      exitFailure
    else
      if runTestM both /=
        "The time is Thu,  1 Jan 1970 00:00:00 UTC\nThe hostname of this computer is \"myhostname\""
      then
        exitFailure
      else
        exitSuccess

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

tests =
  [ fmap (== "The time is Thu,  1 Jan 1970 00:00:00 UTC") myTime
  , fmap (== "The hostname of this computer is \"myhostname\"") myHostName
  , fmap (== "The time is Thu,  1 Jan 1970 00:00:00 UTC\nThe hostname of this computer is \"myhostname\"") both
  ]

main :: IO ()
main = do
  let results = runTestM (sequenceA tests)
  if all (== True) results then
    exitSuccess
    else
    exitFailure

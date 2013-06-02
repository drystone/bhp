module Timer (
    Duration
  , TimerTime(..)
  , Timer(..)
  , duration
  , parseISODate
  , timerTarget
  ) where

import Data.Time.Format (readTime)
import Data.Time.Clock (UTCTime)
import System.Locale (defaultTimeLocale)
import Data.Char (ord)

import Thermometer

type Duration = Int

data TimerTime = TimerDaily Duration
               | TimerWeekly Duration
               | TimerAbsolute UTCTime
    deriving (Show)

data Timer = Timer
    { timerStart        :: TimerTime
    , timerEnd          :: TimerTime
    , timerSetting      :: Either Temperature [Timer] }
    deriving (Show)

duration :: String -> Duration
duration ('P':cs) = duration cs
duration ('T':cs) = duration cs
duration cs = duration' 0 cs
  where duration' a ('D':cs) = 24 * 3600 * a + duration' 0 cs
        duration' a ('H':cs) = 3600 * a + duration' 0 cs
        duration' a ('M':cs) = 60 * a + duration' 0 cs
        duration' a ('S':_) = a
        duration' a (c:cs) = duration' (a * 10 + ord c - ord '0') cs
        duration' _ _ = 0

parseISODate s = readTime defaultTimeLocale "%0Y-%m-%dT%H:%M:%S" s

timerTarget ts = return 17

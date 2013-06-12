module Timer (
    Duration
  , TimerTime(..)
  , Timer(..)
  , duration
  , parseISODate
  , timerTarget
  , time
  ) where

import Data.Time.Format (readTime)
import Data.Time.Clock (UTCTime(..), NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Time.Calendar (toModifiedJulianDay)
import Data.Time.LocalTime (
    LocalTime(localTimeOfDay, localDay)
  , zonedTimeToLocalTime
  , localTimeToUTC
  , getZonedTime
  , getCurrentTimeZone
  , midnight
  , ZonedTime(zonedTimeZone, zonedTimeToLocalTime))
import System.Locale (defaultTimeLocale)
import Data.Char (ord)

import Thermometer

type Duration = Int

data TimerTime = TimerDaily NominalDiffTime
               | TimerWeekly NominalDiffTime
               | TimerAbsolute LocalTime
    deriving (Show)

data Timer = Timer
    { timerStart        :: TimerTime
    , timerEnd          :: TimerTime
    , timerSetting      :: Either Temperature [Timer] }
    deriving (Show)

duration :: String -> NominalDiffTime
duration ('P':cs) = duration cs
duration cs = fromIntegral (duration' 0 cs)
  where duration' a ('D':cs) = 24 * 3600 * a + duration' 0 cs
        duration' a ('T':cs) = a + duration' 0 cs
        duration' a ('H':cs) = 3600 * a + duration' 0 cs
        duration' a ('M':cs) = 60 * a + duration' 0 cs
        duration' a ('S':_) = a
        duration' a (c:cs) = duration' (a * 10 + ord c - ord '0') cs
        duration' _ _ = 0

parseISODate s = readTime defaultTimeLocale "%0Y-%m-%dT%H:%M:%S" s :: LocalTime

time :: TimerTime -> IO UTCTime
time (TimerDaily d) = do 
    zt <- getZonedTime
    return (addUTCTime d (dayStartUTC zt))

time (TimerWeekly d) = do
    zt <- getZonedTime
    let dow = (toModifiedJulianDay (localDay (zonedTimeToLocalTime zt)) + 2) `mod` 7
    let weekStart = addUTCTime (fromIntegral (-(dow * 24 * 60 * 60))) (dayStartUTC zt)
    return (addUTCTime d weekStart)

time (TimerAbsolute t) = do
    z <- getCurrentTimeZone
    return (localTimeToUTC z t)

dayStartUTC :: ZonedTime -> UTCTime
dayStartUTC zt =
    let lt = zonedTimeToLocalTime zt in
    localTimeToUTC (zonedTimeZone zt) ((zonedTimeToLocalTime zt) {localTimeOfDay=midnight})

timerTarget :: [Timer] -> IO (Maybe Temperature)
timerTarget (Timer {timerStart=s, timerEnd=e, timerSetting=set}:ts) = do
    tsTarget <- timerTarget ts
    case tsTarget of
        Just t  -> return (Just t) -- latest match has priority
        Nothing -> do
            use <- boundedTimer s e
            if use then
                case set of
                    Right tts -> timerTarget tts
                    Left temp -> return (Just temp)
            else
                return Nothing
timerTarget _ = return Nothing

-- is current time within range
-- if TimerDaily or TimerWeekly then s>e means rollover from s -> midnight/newweek -> e
boundedTimer s e = do
    st <- time s
    et <- time e
    ct <- getCurrentTime
    return $ test s st et ct
  where test (TimerAbsolute _) s e c = s <= c && c < e
        test _ s e c                 = s <= c && c < e || (e <= s && (s < c || c < e))


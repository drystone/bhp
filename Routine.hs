module Routine
    (
      Routines(..)
    , RoutineSelector(..)
    , Timer
    , loadRoutines
    ) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.XML.Light (parseXML, elChildren)

import Xml
import Thermometer
import Zone
import Timer

type RoutineId = String
type Routine = Map.Map ZoneId [Timer]

data RoutineSelector = RoutineSelector
    { selectorRoutineId :: RoutineId
    , selectorStart     :: TimerTime
    , selectorEnd       :: TimerTime }
    deriving (Show)

data Routines = Routines
    { routinesRoutines  :: Map.Map RoutineId Routine
    , routinesSelectors :: [RoutineSelector]
    }
    deriving (Show)

loadRoutines file zones = do
    routinesEls <- fmap (rootChildren . parseXML) (readFile file)
    return Routines
        { routinesRoutines  = extractRoutines routinesEls zones
        , routinesSelectors = extractWeeklyRoutines routinesEls ++ extractSpecialRoutines routinesEls
        }

extractRoutines es zones = foldr extractRoutine Map.empty (filterElems "daily-routine" es)
  where extractRoutine e = Map.insert (attr "id" e) (foldr extractTimer Map.empty (elChildren e))
        extractTimer e m = Map.insert zid (Timer
            { timerStart        = TimerDaily (duration $ attr "start" e)
            , timerEnd          = TimerDaily (duration $ attr "end" e)
            , timerSetting      = Left (zoneTemperature (attr "state" e) (fromJust $ Map.lookup zid zones))}
            : Map.findWithDefault [] zid m) m
          where zid = attr "zone-id" e

extractWeeklyRoutines es = concatMap (map extract . elChildren) $ filterElems "weekly-routines" es
  where extract e = RoutineSelector
            { selectorRoutineId     = attr "routine-id" e
            , selectorStart         = TimerWeekly (duration $ attr "start" e)
            , selectorEnd           = TimerWeekly (duration $ attr "end" e)}

extractSpecialRoutines es = concatMap (map extract . elChildren) $ filterElems "special-routines" es
  where extract e = RoutineSelector
            { selectorRoutineId     = attr "routine-id" e
            , selectorStart         = TimerAbsolute (parseISODate $ attr "start" e)
            , selectorEnd           = TimerAbsolute (parseISODate $ attr "end" e)}



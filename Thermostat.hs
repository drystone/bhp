module Thermostat
    ( ThermostatId
    , loadThermostats
    , testThermostats
    ) where

import Text.XML.Light (parseXML, findAttr)
import Text.XML.Light.Types (QName(..))
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Control.Monad (foldM)

import Xml
import Thermometer
import Zone
import Timer
import Routine

type ThermostatId = String

data ThermostatTarget = TimerTarget [Timer]
                      | ThermometerTarget ThermometerId
                      | TemperatureTarget Temperature
    deriving (Show)

data Thermostat = Thermostat
    { thermostatId              :: ThermostatId
    , thermostatThermometer     :: ThermometerId
    , thermostatTarget          :: ThermostatTarget
    , thermostatFailsafe        :: Bool
    , thermostatOffset          :: Float
    }
    deriving (Show)

loadThermostats file routines overrides = do
    str <- readFile file
    return [Thermostat
        { thermostatId = attr "id" e
        , thermostatThermometer = attr "thermometer-id" e
        , thermostatTarget = target
            (findAttr (QName "zone-id" Nothing Nothing) e)
            (findAttr (QName "temperature" Nothing Nothing) e)
            (findAttr (QName "target-thermometer-id" Nothing Nothing) e)
        , thermostatFailsafe = 
            case findAttr (QName "failsafe" Nothing Nothing) e of
                Nothing -> True
                Just "over" -> True
                Just "under" -> False
        , thermostatOffset = read $ fromMaybe "0" (findAttr (QName "offset" Nothing Nothing) e)
        } | e <- rootChildren $ parseXML str]
  where
    target (Just zid) _ _           = TimerTarget (zoneTimers zid routines overrides)
    target _ (Just temp) _          = TemperatureTarget (read temp)
    target _ _ (Just thermometerId) = ThermometerTarget thermometerId

zoneTimers zid routines overrides =
    routineSelectors ++ overrideTimers
  where routineSelectors = map (\s -> Timer
            { timerStart    = selectorStart s
            , timerEnd      = selectorEnd s
            , timerSetting  = Right $ Map.findWithDefault [] zid (fromJust $ Map.lookup (selectorRoutineId s) (routinesRoutines routines))
            }) (routinesSelectors routines)
        overrideTimers = Map.findWithDefault [] zid overrides

testThermostats temperatures = foldM fn Map.empty
  where fn m s = testThermostat temperatures s >>= \result -> return $ Map.insert (thermostatId s) result m

testThermostat :: Map.Map ThermometerId (Maybe Temperature) -> Thermostat -> IO Bool
testThermostat temperatures stat =
    case fromJust $ Map.lookup (thermostatThermometer stat) temperatures of
        Nothing   -> log "failed" "unknown" >> return (thermostatFailsafe stat)
        Just temp -> do
            target <- getTarget (thermostatTarget stat)
            case target of
                Nothing -> log (show (temp+offset)) "failed" >> return (thermostatFailsafe stat)
                Just tt -> log (show (temp+offset)) (show tt) >> return (temp+offset > tt)
  where offset = thermostatOffset stat
        getTarget (TimerTarget tt)       = timerTarget tt
        getTarget (ThermometerTarget tt) = return $ fromJust $ Map.lookup tt temperatures
        getTarget (TemperatureTarget tt) = return $ Just tt
        log cur tgt = putStrLn ("Thermostat id: " ++ thermostatId stat ++ ", current: " ++ cur ++ ", target: " ++ tgt)


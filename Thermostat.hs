module Thermostat
    (
      Thermostat(..)
    , ThermostatId
    , ThermostatTarget(..)
    , loadThermostats
    , testThermostat
    ) where

import Text.XML.Light (parseXML, findAttr)
import Text.XML.Light.Types (QName(..))
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import Xml
import Thermometer
import Zone
import Timer
import Routine

type ThermostatId = String

data ThermostatTarget = TimerTarget [Timer]
                      | ThermometerTarget Thermometer
                      | TemperatureTarget Temperature
    deriving (Show)

data Thermostat = Thermostat
    { thermostatThermometer     :: Thermometer
    , thermostatTarget          :: ThermostatTarget }
    deriving (Show)

loadThermostats file thermometers routines overrides = do
    str <- readFile file
    return $ foldr extract Map.empty (rootChildren $ parseXML str)
  where extract e = Map.insert (attr "id" e) Thermostat
            { thermostatThermometer = fromJust $ Map.lookup (attr "thermometer-id" e) thermometers
            , thermostatTarget      = target
                (findAttr (QName "zone-id" Nothing Nothing) e)
                (findAttr (QName "temperature" Nothing Nothing) e)
                (findAttr (QName "target-thermometer-id" Nothing Nothing) e)
            }
          where target (Just zid) _ _           = TimerTarget (zoneTimers zid routines overrides)
                target _ (Just temp) _          = TemperatureTarget $ read temp
                target _ _ (Just thermometerId) = ThermometerTarget $ fromJust $ Map.lookup thermometerId thermometers

zoneTimers zid routines overrides =
    routineSelectors ++ overrideTimers
  where routineSelectors = map (\s -> Timer
            { timerStart    = selectorStart s
            , timerEnd      = selectorEnd s
            , timerSetting  = Right $ Map.findWithDefault [] zid (fromJust $ Map.lookup (selectorRoutineId s) (routinesRoutines routines))
            }) (routinesSelectors routines)
        overrideTimers = Map.findWithDefault [] zid overrides

testThermostat Thermostat {thermostatThermometer=thermometer, thermostatTarget=(TimerTarget target)} = do
    temperature <- readThermometer thermometer
    targetTemperature <- timerTarget target
    return (temperature > targetTemperature)
testThermostat Thermostat {thermostatThermometer=thermometer, thermostatTarget=(ThermometerTarget target)} = do
    temperature <- readThermometer thermometer
    targetTemperature <- readThermometer target
    return (temperature > targetTemperature)
testThermostat Thermostat {thermostatThermometer=thermometer, thermostatTarget=(TemperatureTarget target)} = do
    temperature <- readThermometer thermometer
    return (temperature > target)


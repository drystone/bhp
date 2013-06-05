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
import Data.Maybe (fromJust, fromMaybe)

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
    { thermostatId              :: ThermostatId
    , thermostatThermometer     :: Thermometer
    , thermostatTarget          :: ThermostatTarget
    , thermostatFailsafe        :: Bool
    , thermostatOffset          :: Float
    }
    deriving (Show)

loadThermostats file thermometers routines overrides = do
    str <- readFile file
    return $ foldr extract Map.empty (rootChildren $ parseXML str)
  where extract e = Map.insert id Thermostat
            { thermostatId          = id
            , thermostatThermometer = fromJust $ Map.lookup (attr "thermometer-id" e) thermometers
            , thermostatTarget      = target
                (findAttr (QName "zone-id" Nothing Nothing) e)
                (findAttr (QName "temperature" Nothing Nothing) e)
                (findAttr (QName "target-thermometer-id" Nothing Nothing) e)
            , thermostatFailsafe    = 
                case findAttr (QName "failsafe" Nothing Nothing) e of
                    Nothing -> True
                    Just "over" -> True
                    Just "under" -> False
            , thermostatOffset      = read $ fromMaybe "0" (findAttr (QName "offset" Nothing Nothing) e)
            }
          where id = attr "id" e
                target (Just zid) _ _           = TimerTarget (zoneTimers zid routines overrides)
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

testThermostat stat = do
    mt <- readThermometer $ thermostatThermometer stat
    case mt of
        Nothing   -> log "failed" "unknown" >> return (thermostatFailsafe stat)
        Just temp -> do
            let offset = thermostatOffset stat
            target <- getTarget $ thermostatTarget stat
            case target of
                Nothing -> log (show (temp+offset)) "failed" >> return (thermostatFailsafe stat)
                Just tt -> do
                    log (show (temp+offset)) (show tt)
                    return (temp+offset > tt)
  where getTarget (TimerTarget tt)       = timerTarget tt
        getTarget (ThermometerTarget tt) = readThermometer tt
        getTarget (TemperatureTarget tt) = return $ Just tt
        log cur tgt = putStrLn ("Thermostat id: " ++ thermostatId stat ++ ", current: " ++ cur ++ ", target: " ++ tgt)

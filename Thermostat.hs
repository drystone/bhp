module Thermostat
    ( ThermostatId
    , loadThermostats
    , testThermostats
    , getThermostatStateXml
    ) where

import qualified Text.XML.Light as X
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Control.Monad (foldM)
import Data.List (find)

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
            (X.findAttr (X.QName "zone-id" Nothing Nothing) e)
            (X.findAttr (X.QName "temperature" Nothing Nothing) e)
            (X.findAttr (X.QName "target-thermometer-id" Nothing Nothing) e)
        , thermostatFailsafe = 
            case X.findAttr (X.QName "failsafe" Nothing Nothing) e of
                Nothing -> True
                Just "over" -> True
                Just "under" -> False
        , thermostatOffset = read $ fromMaybe "0" (X.findAttr (X.QName "offset" Nothing Nothing) e)
        } | e <- rootChildren $ X.parseXML str]
  where
    target (Just zid) _ _           = TimerTarget (zoneTimers zid routines overrides)
    target _ (Just temp) _          = TemperatureTarget (read temp)
    target _ _ (Just thermometerId) = ThermometerTarget thermometerId

zoneTimers zid routines overrides =
    routineSelectors ++ overrideTimer
  where routineSelectors = map (\s -> Timer
            { timerStart    = selectorStart s
            , timerEnd      = selectorEnd s
            , timerSetting  = Right $ Map.findWithDefault [] zid (fromJust $ Map.lookup (selectorRoutineId s) (routinesRoutines routines))
            }) (routinesSelectors routines)
        overrideTimer = case Map.lookup zid overrides of
            Nothing -> []
            Just t -> [t]

testThermostats thermometers = foldM fn Map.empty
  where fn m s = testThermostat thermometers s >>= \result -> return $ Map.insert (thermostatId s) result m

testThermostat :: [Thermometer] -> Thermostat -> IO Bool
testThermostat thermometers stat =
    case getReading (thermostatThermometer stat) of
        Nothing   -> log "failed" "unknown" >> return (thermostatFailsafe stat)
        Just temp -> do
            target <- getTarget (thermostatTarget stat)
            case target of
                Nothing -> log (show (temp+offset)) "failed" >> return (thermostatFailsafe stat)
                Just tt -> log (show (temp+offset)) (show tt) >> return (temp+offset > tt)
  where offset = thermostatOffset stat
        getTarget (TimerTarget tt)       = timerTarget tt
        getTarget (ThermometerTarget tt) = return $ getReading tt
        getTarget (TemperatureTarget tt) = return $ Just tt
        getReading tid = thermometerReading $ fromJust $ find (\t -> thermometerId t == tid) thermometers
        log cur tgt = putStrLn ("Thermostat id: " ++ thermostatId stat ++ ", current: " ++ cur ++ ", target: " ++ tgt)

getThermostatStateXml :: Map.Map ThermostatId Bool -> String
getThermostatStateXml ts = 
    X.ppTopElement $ X.Element (X.QName "thermostat-states" Nothing Nothing) [] 
        [X.Elem (X.Element (X.QName "thermostat-state" Nothing Nothing) 
            [ X.Attr (X.QName "thermostat-id" Nothing Nothing) (fst t)]
            [ X.Text (X.CData X.CDataText (if snd t then "over" else "under") Nothing) ]
            Nothing)
        | t <- Map.toList ts] Nothing

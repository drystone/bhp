module Main (main) where

import Control.Monad

import Thermometer
import Zone
import Override
import Thermostat
import Routine
import Control

main = do
    zones <- loadZones "zones.xml"
    routines <- loadRoutines "routines.xml" zones
    overrides <- loadOverrides "overrides.xml" zones
    thermometers <- loadThermometers "thermometers.xml" "/mnt/1wire" "/mnt/arexx"
    thermostats <- loadThermostats "thermostats.xml" thermometers routines overrides
    controls <- loadControls "controls.xml" thermostats
    loop controls

loop :: Controls -> IO ()
loop controls = do
    let _ = updateControls controls
    loop controls
    return ()

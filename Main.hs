module Main (main) where

import System.Console.GetOpt (OptDescr(Option), ArgDescr(ReqArg, NoArg), usageInfo, getOpt, ArgOrder(RequireOrder))
import System.Environment (getProgName, getArgs)
import System.Exit (exitSuccess)
import Data.List (intercalate)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

import Thermometer
import Zone
import Override
import Thermostat
import Routine
import Control

import qualified Data.Map as Map
import Data.Maybe

data Flag = ArexxDir (Maybe String)
            | OwDir (Maybe String)
            | UdinDir (Maybe String)
            | Fht8vDir (Maybe String)

data Options = Options { optConfigDir   :: String
                       , optRunDir      :: String
                       , optLibDir      :: String
                       , optArexxDir    :: Maybe String
                       , optOwDir       :: Maybe String
                       , optUdinDir     :: Maybe String
                       , optFht8vDir    :: Maybe String
                       }

startOptions = Options { optConfigDir   = "/etc/bhp"
                       , optRunDir      = "/var/run/bhp"
                       , optLibDir      = "/var/lib/bhp"
                       , optArexxDir    = Nothing
                       , optOwDir       = Nothing
                       , optUdinDir     = Nothing
                       , optFht8vDir    = Nothing
                       }

options =
    [ Option "c" ["config-dir"]
        (ReqArg
            (\arg opt -> return opt { optConfigDir = arg })
            "DIRECTORY")
        "configuration directory (default /etc/bhp)"
 
    , Option "r" ["runtime-dir"]
        (ReqArg
            (\arg opt -> return opt { optRunDir = arg })
            "DIRECTORY")
        "directory for runtime state files (default /var/run/bhp)"
 
    , Option "l" ["lib-dir"]
        (ReqArg
            (\arg opt -> return opt { optLibDir = arg })
            "DIRECTORY")
        "drirectory for persistent state files (default /var/lib/bhp)"
 
    , Option "a" ["arexx-dir"]
        (ReqArg
            (\arg opt -> return opt { optArexxDir = Just arg })
            "DIRECTORY")
        "arexx temperature sensor mount point"
 
    , Option "o" ["ow-dir"]
        (ReqArg
            (\arg opt -> return opt { optOwDir = Just arg })
            "DIRECTORY")
        "1wire temperature sensor mount point"
 
    , Option "u" ["udin-dir"]
        (ReqArg
            (\arg opt -> return opt { optUdinDir = Just arg })
            "DIRECTORY")
        "udin switch mount point"
 
    , Option "f" ["fht-dir"]
        (ReqArg
            (\arg opt -> return opt { optFht8vDir = Just arg })
            "DIRECTORY")
        "fht8v (CUL) device mount point"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                putStrLn (usageInfo prg options)
                exitSuccess))
        "Show help"
    ]

main = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    let Options { optConfigDir  = configDir
                , optLibDir     = libDir
                , optRunDir     = runDir
                , optArexxDir   = arexxDir
                , optOwDir      = owDir
                , optUdinDir    = udinDir
                , optFht8vDir   = fht8vDir } = opts

    zones <- loadZones (configDir ++ "/zones.xml")
    routines <- loadRoutines (configDir ++ "/routines.xml") zones
    overrides <- loadOverrides (libDir ++ "/overrides.xml") zones
    thermometers <- loadThermometers (configDir ++ "/thermometers.xml") owDir arexxDir
    thermostats <- loadThermostats (configDir ++ "/thermostats.xml") routines overrides
    controls <- loadControls (configDir ++ "/controls.xml") udinDir fht8vDir
    forever (loop thermometers thermostats controls)
  where
    loop thermometers thermostats controls = do
        temperatures <- readThermometers thermometers
        thermostatStates <- testThermostats temperatures thermostats
        controlStates <- evalControlConditions thermostatStates controls
        actuateControls controlStates controls
        threadDelay 5000000
        return ()

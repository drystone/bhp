module Main (main) where

import System.Console.GetOpt (OptDescr(Option), ArgDescr(ReqArg, NoArg), usageInfo, getOpt, ArgOrder(RequireOrder))
import System.Environment (getProgName, getArgs)
import System.Exit (exitSuccess)
import Data.List (intercalate)
import Control.Concurrent (threadDelay, forkIO, putMVar, tryTakeMVar, newEmptyMVar)
import Control.Monad (forever)
import System.Directory (renameFile)
import qualified Data.ByteString.Char8 as BS
import System.INotify (initINotify, addWatch, EventVariety(..))

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
    mvar <- newEmptyMVar
    inotify <- initINotify
    addWatch inotify [MoveIn] (optLibDir opts) $ \e -> putMVar mvar e
    daemon opts mvar
  where
    daemon opts mvar = do
        let Options { optConfigDir  = configDir
                    , optLibDir     = libDir
                    , optRunDir     = runDir
                    , optArexxDir   = arexxDir
                    , optOwDir      = owDir
                    , optUdinDir    = udinDir
                    , optFht8vDir   = fht8vDir } = opts
        putStrLn "(Re)loading configuration"
        zones <- loadZones (configDir ++ "/zones.xml")
        routines <- loadRoutines (configDir ++ "/routines.xml") zones
        overrides <- loadOverrides (libDir ++ "/overrides.xml") zones
        thermometers <- loadThermometers (configDir ++ "/thermometers.xml") owDir arexxDir
        thermostats <- loadThermostats (configDir ++ "/thermostats.xml") routines overrides
        controls <- loadControls (configDir ++ "/controls.xml") udinDir fht8vDir
        loop thermometers thermostats controls runDir
        daemon opts mvar
      where
        loop thermometers thermostats controls runDir = do
            temperatures <- readThermometers thermometers
            saveState runDir "temperatures.xml" $ getTemperatureXml temperatures
            thermostatStates <- testThermostats temperatures thermostats
            saveState runDir "state.xml" $ getThermostatStateXml thermostatStates
            controlStates <- evalControlConditions thermostatStates controls
            saveState runDir "control-state.xml" $ getControlStateXml controlStates
            actuateControls controlStates controls
            reload <- tryTakeMVar mvar
            case reload of
                Nothing -> threadDelay (5*10^6) >> loop thermometers thermostats controls runDir
                otherwise -> return ()

-- write to a temporary file and rename into place
saveState :: FilePath -> FilePath -> String -> IO ()
saveState dir fnam s = do
    BS.writeFile tnam (BS.pack s)
    renameFile tnam (dir ++ ('/':fnam))
  where
    tnam = dir ++ "/.bhp.tmp"
 

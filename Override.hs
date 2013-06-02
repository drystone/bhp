module Override 
    (
      loadOverrides
    ) where

import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Text.XML.Light (parseXML)

import Xml
import Zone
import Timer

loadOverrides file zones = do
    str <- readFile file
    return $ foldr extractOverride Map.empty (rootChildren $ parseXML str)
  where extractOverride e m = Map.insert zid (Timer
            { timerStart    = TimerAbsolute (parseISODate $ attr "start" e)
            , timerEnd      = TimerAbsolute (parseISODate $ attr "end" e)
            , timerSetting  = Left $ zoneTemperature (attr "state" e) (fromJust $ Map.lookup zid zones)
            } : Map.findWithDefault [] zid m) m
          where zid = attr "zone-id" e


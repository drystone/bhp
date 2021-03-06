module Zone
    (
      ZoneId
    , Zone(zoneDefault)
    , loadZones
    , zoneTemperature
    ) where

import qualified Data.Map as Map
import Text.XML.Light (parseXML)

import Xml
import Thermometer

type ZoneId = String

data Zone = Zone
    { zoneOff       :: Temperature
    , zoneStandby   :: Temperature
    , zoneOn        :: Temperature
    , zoneDefault   :: Temperature
    } deriving (Show)

loadZones filename = do
    str <- readFile filename
    return $ foldr extract Map.empty (rootChildren $ parseXML str)
  where extract e = Map.insert (attr "id" e) Zone
            { zoneOff       = read $ attr "off" e
            , zoneStandby   = read $ attr "standby" e
            , zoneOn        = read $ attr "on" e
            , zoneDefault   = read $ attr "off" e   -- TODO allow default value in zones.xml
            }

zoneTemperature "off"       = zoneOff
zoneTemperature "standby"   = zoneStandby
zoneTemperature "on"        = zoneOn

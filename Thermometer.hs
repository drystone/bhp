module Thermometer
    (
      Temperature
    , Thermometer
    , loadThermometers
    , readThermometer
    ) where

import System.Directory (getDirectoryContents)
import Text.Regex.Posix ((=~))
import qualified Data.Map as Map
import Text.XML.Light (parseXML, findAttr, QName(..))
import System.IO (readFile)

import Xml (rootChildren, attr)

type Temperature = Float
type ThermometerDeviceId = String

data ThermometerDevice = OwDevice (Maybe FilePath)
                       | ArexxDevice FilePath
    deriving (Show)

data Thermometer = Thermometer
    { thermometerDevice     :: ThermometerDevice
    , thermometerCorrection :: Maybe Float }
    deriving (Show)

locateOwThermometers path = do
    dir <- getDirectoryContents path
    if "temperature" `elem` dir
        then do
            id <- readFile (path ++ "/address")
            return [(id, path ++ "/temperature")]
        else do
            pss <- mapM (locateOwThermometers . (\x -> path ++ "/" ++ x))
                   $ filter (=~ "^(main|aux|[0-9A-F]{2}\\.[0-9A-F]{12})$") dir
            return (concat pss)

loadThermometers file owPath arexxPath = do
    owPaths <- locateOwThermometers owPath
    str <- readFile file
    return $ foldr (extract owPaths) Map.empty (rootChildren $ parseXML str)
  where
    extract owPaths e = Map.insert (attr "id" e) Thermometer
        { thermometerDevice = thermometerDevice (attr "type" e) (attr "device-id" e)
        , thermometerCorrection = fmap read $ findAttr (QName "correction" Nothing Nothing) e
        }
      where 
        thermometerDevice "1wire" id = OwDevice (Map.lookup id owPathMap)
        thermometerDevice "arexx" id = ArexxDevice (arexxPath ++ "/" ++ id)
        owPathMap = Map.fromList(owPaths)

readThermometer :: Thermometer -> IO Temperature
readThermometer Thermometer {thermometerDevice=(OwDevice Nothing)} = return 0
readThermometer Thermometer {thermometerDevice=OwDevice (Just path), thermometerCorrection=c} =
    readThermometer' path c
readThermometer Thermometer {thermometerDevice=ArexxDevice path, thermometerCorrection=c} =
    readThermometer' path c

readThermometer' path c = do
    temperature <- readFile path
    return (correction c + read temperature)
  where 
    correction (Just c) = c
    correction Nothing = 0


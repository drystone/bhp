module Thermometer
    (
      Temperature
    , ThermometerId
    , loadThermometers
    , readThermometers
    , getTemperatureXml
    ) where

import System.Directory (getDirectoryContents)
import Text.Regex.Posix ((=~))
import qualified Data.Map as Map
import qualified Text.XML.Light as X
import System.IO (readFile)
import Data.Traversable
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BS
import Data.Text.Read (hexadecimal)
import Data.Text (pack)
import Control.Exception (try, IOException)
import Control.Monad (foldM)

import Xml (rootChildren, attr)

type Temperature = Float
type ThermometerId = String
type ThermometerDeviceId = String

data ThermometerDevice = OwDevice (Maybe FilePath)
                       | ArexxDevice FilePath
    deriving (Show)

data Thermometer = Thermometer
    { thermometerId         :: ThermometerId
    , thermometerDevice     :: ThermometerDevice
    , thermometerCorrection :: Float }
    deriving (Show)

locateOwThermometers dir = do
    cs <- getDirectoryContents dir
    if "temperature" `elem` cs
        then do
            id <- readFile (dir ++ "/address")
            return [(id, dir ++ "/temperature")]
        else do
            pss <- Prelude.mapM (locateOwThermometers . (\x -> dir ++ ('/':x)))
                   $ filter (=~ "^(main|aux|[0-9A-F]{2}\\.[0-9A-F]{12})$") cs
            return (concat pss)

loadThermometers file owDir arexxDir = do
    -- sequence flips Maybe (IO []) -> IO (Maybe [])
    owPaths <- Data.Traversable.sequence $ fmap locateOwThermometers owDir
    str <- readFile file
    return (thermometers (fmap Map.fromList owPaths) (rootChildren (X.parseXML str)))
  where
    thermometers owPathMap = map (\e -> Thermometer
        { thermometerId = attr "id" e
        , thermometerDevice = thermometerDevice (attr "type" e) (attr "device-id" e)
        , thermometerCorrection = maybe 0 read (X.findAttr (X.QName "correction" Nothing Nothing) e)
        })
      where
        thermometerDevice "1wire" id =
            case owPathMap of
                Just m   -> OwDevice (Map.lookup id m)
                Nothing  -> error "Cannot use 1wire device without specifying 1wire mountpoint with -o"
        thermometerDevice "arexx" id =
            case arexxDir of
                Just dir -> ArexxDevice (dir ++ ('/':id))
                Nothing  -> error "Cannot use Arexx device without specifying arexx mountpoint with -a"

readThermometers = foldM f Map.empty
  where f m d = readThermometer d >>= \t -> return $ Map.insert (thermometerId d) t m

readThermometer :: Thermometer -> IO (Maybe Temperature)

readThermometer Thermometer {thermometerDevice=(OwDevice Nothing)} = do
    putStrLn "ow device path not found"
    return Nothing

readThermometer Thermometer {thermometerDevice=OwDevice (Just path), thermometerCorrection=c} = do
    result <- readThermometerRaw path
    return (fmap (\s -> read (BS.unpack s) + c) result)

readThermometer Thermometer {thermometerDevice=ArexxDevice path, thermometerCorrection=c} = do
    result <- readThermometerRaw path
    return (fmap unhex result)
  where 
    unhex s = 
        let Right (n, _) = hexadecimal (pack $ BS.unpack s) in
        fromIntegral n * 0.0078 + c

readThermometerRaw path = do
    result <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
    case result of
        Right t -> return (Just t)
        Left e  -> return Nothing

getTemperatureXml :: Map.Map ThermometerId (Maybe Temperature) -> String
getTemperatureXml ts = 
    X.ppTopElement $ X.Element (X.QName "temperatures" Nothing Nothing) [] 
        [X.Elem (X.Element (X.QName "temperature" Nothing Nothing) 
            [ X.Attr (X.QName "thermometer-id" Nothing Nothing) (fst t)]
            [ X.Text (X.CData X.CDataText (tStr (snd t)) Nothing) ]
            Nothing)
        | t <- Map.toList ts] Nothing
  where
    tStr Nothing = "Unknown"
    tStr (Just t) = show t

module Thermometer
    (
      Temperature
    , ThermometerId
    , loadThermometers
    , readThermometers
    ) where

import System.Directory (getDirectoryContents)
import Text.Regex.Posix ((=~))
import qualified Data.Map as Map
import Text.XML.Light (parseXML, findAttr, QName(..))
import System.IO (readFile)
import Data.Traversable
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BS
import Data.Text.Read (hexadecimal)
import Data.Text (pack)
import Control.Exception (try, SomeException)
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
    return (thermometers (fmap Map.fromList owPaths) (rootChildren (parseXML str)))
  where
    thermometers owPathMap = map (\e -> Thermometer
        { thermometerId = attr "id" e
        , thermometerDevice = thermometerDevice (attr "type" e) (attr "device-id" e)
        , thermometerCorrection = maybe 0 read (findAttr (QName "correction" Nothing Nothing) e)
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
    result <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
    case result of
        Left e  -> print e >> return Nothing
        Right t -> return (Just t)

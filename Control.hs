module Control
    ( loadControls
    , evalControlConditions
    , actuateControls
    , getControlStateXml
    ) where

import qualified Text.XML.Light as X
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (find)
import qualified Data.ByteString.Char8 as BS
import Control.Monad (mapM_, foldM)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Data.Time.Clock (UTCTime(UTCTime, utctDay, utctDayTime), diffUTCTime, getCurrentTime)

import Thermostat
import Xml

type ControlId = String
type ControlPath = String

data ControlState = ControlStateOn | ControlStateOff
    deriving (Eq, Show)

data Control = Control
    { controlId         :: ControlId
    , controlPath       :: ControlPath
    , controlCondition  :: ControlCondition
    , controlState      :: ControlState
    , controlChange     :: UTCTime }
    deriving (Show)

data ControlCondition = ControlConditionNot ControlCondition
                      | ControlConditionOr [ControlCondition]
                      | ControlConditionAnd [ControlCondition]
                      | ControlConditionOver ThermostatId
                      | ControlConditionOn Control
                      | NoControlCondition
    deriving (Show)

controlDefault = Control {
    controlId = ""
  , controlPath = ""
  , controlCondition = NoControlCondition
  , controlState = ControlStateOff
  , controlChange = UTCTime (ModifiedJulianDay 0) 0
}

loadControls file udinDir fht8vDir = do
    str <- readFile file
    let rootEl = last $ X.onlyElems $ X.parseXML str
    return $ map (extractControl rootEl) (findControlEls rootEl)
  where extractControl rootEl el = controlDefault
            { controlId = attr "id" el
            , controlPath = controlPath (attr "type" el) (attr "device-code" el)
            , controlCondition  = extractCondition rootEl $ head $ X.elChildren el }

        extractCondition rootEl el = case X.qName $ X.elName el of
            "not"   -> ControlConditionNot $ extractCondition rootEl (head $ X.elChildren el)
            "or"    -> ControlConditionOr $ map (extractCondition rootEl) (X.elChildren el)
            "and"   -> ControlConditionAnd $ map (extractCondition rootEl) (X.elChildren el)
            "over"  -> ControlConditionOver $ attr "thermostat-id" el
            "under" -> ControlConditionNot $ ControlConditionOver $ attr "thermostat-id" el
            "on"    -> ControlConditionOn $ extractControl rootEl $ findControlEl (attr "control-id" el) rootEl
            "off"   -> ControlConditionNot $ ControlConditionOn $ extractControl rootEl $ findControlEl (attr "control-id" el) rootEl
            "true"  -> extractMacro rootEl $ findMacroEl (attr "macro-id" el) rootEl
            "false" -> ControlConditionNot $ extractMacro rootEl $ findMacroEl (attr "macro-id" el) rootEl

        extractMacro rootEl el = extractCondition rootEl (head $ X.elChildren el)

        controlPath "udin"  id = 
            case udinDir of
                Just dir -> dir ++ ('/':id)
                Nothing  -> error "Cannot use udin device without specifying udin mountpoint with -u"
        controlPath "fht8v" id =
            case fht8vDir of
                Just dir -> dir ++ ('/':id)
                Nothing  -> error "Cannot use fht8v device without specifying fht8v mountpoint with -f"

        findControlEls = X.filterChildren (\e -> X.qName (X.elName e) == "control") 
        findControlEl id rootEl = fromJust $ find (\e -> attr "id" e == id) $ findControlEls rootEl

        findMacroEls = X.filterChildren (\e -> X.qName (X.elName e) == "macro") 
        findMacroEl id rootEl = fromJust $ find (\e -> attr "id" e == id) $ findMacroEls rootEl


evalControlConditions :: Map.Map ThermostatId Bool -> [Control] -> IO ([Control])
evalControlConditions thermostatStates = mapM ecc
  where
    ecc c = do 
        bState <- evalCondition (controlCondition c)
        let newState = if bState then ControlStateOn else ControlStateOff
        ct <- getCurrentTime
        return $ c { controlState    = newState
                   , controlChange   = if newState /= controlState c then ct else controlChange c }
      where
    evalCondition (ControlConditionNot c) = fmap not (evalCondition c)
    evalCondition (ControlConditionOr cs) = fmap (True `elem`) (mapM evalCondition cs)
    evalCondition (ControlConditionAnd cs) = fmap (not . (False `elem`)) (mapM evalCondition cs)
    evalCondition (ControlConditionOver t) = return $ fromJust $ Map.lookup t thermostatStates
    evalCondition (ControlConditionOn c) = return $ controlState c == ControlStateOn

actuateControls :: [Control] -> IO ()
actuateControls = mapM_ actuate
  where actuate c = log >> BS.writeFile (controlPath c) (BS.pack $ if controlState c == ControlStateOn then "1" else "0")
          where log = putStrLn ("Control id: " ++ controlId c ++ ", state: " ++ show (controlState c))

getControlStateXml :: [Control] -> String
getControlStateXml cs = 
    X.ppTopElement $ X.Element (X.QName "control-states" Nothing Nothing) [] 
        [X.Elem (X.Element (X.QName "control-state" Nothing Nothing) 
            [ X.Attr (X.QName "id" Nothing Nothing) (controlId c)]
            [ X.Text (X.CData X.CDataText (if controlState c == ControlStateOn then "on" else "off") Nothing) ]
            Nothing)
        | c <- cs] Nothing


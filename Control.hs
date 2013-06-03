module Control
    (
      Controls
    , loadControls
    , updateControls
    ) where

import Text.XML.Light (parseXML, elChildren, filterChildren, onlyElems)
import Text.XML.Light.Types (qName, elName)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (find)

import Thermostat
import Xml

type Controls = [Control]
type ControlId = String
type ControlDeviceId = String

data ControlState   = ControlStateOn
                    | ControlStateOff
    deriving (Eq, Show)

data Control = Control
    { controlDevicePath :: ControlDeviceId
    , controlCondition  :: ControlCondition }
    deriving (Show)

data ControlCondition = ControlConditionNot ControlCondition
                      | ControlConditionOr [ControlCondition]
                      | ControlConditionAnd [ControlCondition]
                      | ControlConditionOver Thermostat
                      | ControlConditionOn Control
    deriving (Show)

loadControls file udinPath fht8vPath thermostats = do
    str <- readFile file
    let rootEl = last $ onlyElems $ parseXML str
    return $ map (extractControl rootEl) (findControlEls rootEl)
  where extractControl rootEl el = Control
            { controlDevicePath = controlDevicePath (attr "type" el) (attr "device-code" el)
            , controlCondition  = extractCondition rootEl $ head $ elChildren el }
        extractCondition rootEl el = case qName $ elName el of
            "not"   -> ControlConditionNot $ extractCondition rootEl (head $ elChildren el)
            "or"    -> ControlConditionOr $ map (extractCondition rootEl) (elChildren el)
            "and"   -> ControlConditionAnd $ map (extractCondition rootEl) (elChildren el)
            "over"  -> ControlConditionOver $ fromJust $ Map.lookup (attr "thermostat-id" el) thermostats
            "under" -> ControlConditionNot $ ControlConditionOver $ fromJust $ Map.lookup (attr "thermostat-id" el) thermostats
            "on"    -> ControlConditionOn $ extractControl rootEl $ findControlEl (attr "control-id" el) rootEl
            "off"   -> ControlConditionNot $ ControlConditionOn $ extractControl rootEl $ findControlEl (attr "control-id" el) rootEl
            "true"  -> extractMacro rootEl $ findMacroEl (attr "macro-id" el) rootEl
            "false" -> ControlConditionNot $ extractMacro rootEl $ findMacroEl (attr "macro-id" el) rootEl
        extractMacro rootEl el = extractCondition rootEl (head $ elChildren el)
        controlDevicePath "udin"  id = udinPath ++ ('/':id)
        controlDevicePath "fht8v" id = fht8vPath ++ ('/':id)

evalCondition (ControlConditionNot c) = fmap not (evalCondition c)
evalCondition (ControlConditionOr cs) = fmap (True `elem`) (mapM evalCondition cs)
evalCondition (ControlConditionAnd cs) = fmap not (fmap (False `elem`) (mapM evalCondition cs))
evalCondition (ControlConditionOver t) = testThermostat t
evalCondition (ControlConditionOn c) = testControl c

findControlEls = filterChildren (\e -> qName (elName e) == "control") 
findControlEl id rootEl = fromJust $ find (\e -> attr "id" e == id) $ findControlEls rootEl

findMacroEls = filterChildren (\e -> qName (elName e) == "macro") 
findMacroEl id rootEl = fromJust $ find (\e -> attr "id" e == id) $ findMacroEls rootEl

updateControls = map actuate
  where actuate c = do
            state <- evalCondition (controlCondition c)
            if state then
                writeFile (controlDevicePath c) "1"
            else
                writeFile (controlDevicePath c) "0"

testControl control = do
    state <- readFile (controlDevicePath control)
    case state of
        ('1':_) -> return True
        _       -> return False

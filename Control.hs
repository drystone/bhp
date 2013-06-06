module Control
    ( loadControls
    , evalControlConditions
    , actuateControls
    ) where

import Text.XML.Light (parseXML, elChildren, filterChildren, onlyElems)
import Text.XML.Light.Types (qName, elName)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (find)
import qualified Data.ByteString.Char8 as BS
import Control.Monad (mapM_, foldM)

import Thermostat
import Xml

type ControlId = String
type ControlPath = String

data ControlState   = ControlStateOn
                    | ControlStateOff
    deriving (Eq, Show)

data Control = Control
    { controlId         :: ControlId
    , controlPath :: ControlPath
    , controlCondition  :: ControlCondition }
    deriving (Show)

data ControlCondition = ControlConditionNot ControlCondition
                      | ControlConditionOr [ControlCondition]
                      | ControlConditionAnd [ControlCondition]
                      | ControlConditionOver ThermostatId
                      | ControlConditionOn Control
    deriving (Show)

loadControls file udinDir fht8vDir = do
    str <- readFile file
    let rootEl = last $ onlyElems $ parseXML str
    return $ map (extractControl rootEl) (findControlEls rootEl)
  where extractControl rootEl el = Control
            { controlId = attr "id" el
            , controlPath = controlPath (attr "type" el) (attr "device-code" el)
            , controlCondition  = extractCondition rootEl $ head $ elChildren el }

        extractCondition rootEl el = case qName $ elName el of
            "not"   -> ControlConditionNot $ extractCondition rootEl (head $ elChildren el)
            "or"    -> ControlConditionOr $ map (extractCondition rootEl) (elChildren el)
            "and"   -> ControlConditionAnd $ map (extractCondition rootEl) (elChildren el)
            "over"  -> ControlConditionOver $ attr "thermostat-id" el
            "under" -> ControlConditionNot $ ControlConditionOver $ attr "thermostat-id" el
            "on"    -> ControlConditionOn $ extractControl rootEl $ findControlEl (attr "control-id" el) rootEl
            "off"   -> ControlConditionNot $ ControlConditionOn $ extractControl rootEl $ findControlEl (attr "control-id" el) rootEl
            "true"  -> extractMacro rootEl $ findMacroEl (attr "macro-id" el) rootEl
            "false" -> ControlConditionNot $ extractMacro rootEl $ findMacroEl (attr "macro-id" el) rootEl

        extractMacro rootEl el = extractCondition rootEl (head $ elChildren el)

        controlPath "udin"  id = 
            case udinDir of
                Just dir -> dir ++ ('/':id)
                Nothing  -> error "Cannot use udin device without specifying udin mountpoint with -u"
        controlPath "fht8v" id =
            case fht8vDir of
                Just dir -> dir ++ ('/':id)
                Nothing  -> error "Cannot use fht8v device without specifying fht8v mountpoint with -f"

        findControlEls = filterChildren (\e -> qName (elName e) == "control") 
        findControlEl id rootEl = fromJust $ find (\e -> attr "id" e == id) $ findControlEls rootEl

        findMacroEls = filterChildren (\e -> qName (elName e) == "macro") 
        findMacroEl id rootEl = fromJust $ find (\e -> attr "id" e == id) $ findMacroEls rootEl


evalControlConditions :: Map.Map ThermostatId Bool -> [Control] -> IO (Map.Map ControlId Bool)
evalControlConditions thermostatStates = foldM fn Map.empty
  where
    fn m c = evalCondition (controlCondition c) >>= \s -> return $ Map.insert (controlId c) s m

    evalCondition (ControlConditionNot c) = fmap not (evalCondition c)
    evalCondition (ControlConditionOr cs) = fmap (True `elem`) (mapM evalCondition cs)
    evalCondition (ControlConditionAnd cs) = fmap (not . (False `elem`)) (mapM evalCondition cs)
    evalCondition (ControlConditionOver t) = return $ fromJust $ Map.lookup t thermostatStates
    evalCondition (ControlConditionOn c) = testControl c

    testControl control = do
        state <- BS.readFile (controlPath control)
        case BS.unpack state of
            ('1':_) -> return True
            _       -> return False

actuateControls :: Map.Map ControlId Bool -> [Control] -> IO ()
actuateControls controlStates = mapM_ actuate
  where actuate c = log >> BS.writeFile (controlPath c) (BS.pack $ if state then "1" else "0")
          where state = fromJust (Map.lookup (controlId c) controlStates)
                log = putStrLn ("Control id: " ++ controlId c ++ ", state: " ++ show state)


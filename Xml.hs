module Xml
  (
    rootChildren
  , attr
  , filterElems
  ) where

import Text.XML.Light (onlyElems, elChildren, elName, findAttr)
import Text.XML.Light.Types (QName(..))
import Data.Maybe (fromJust)

rootChildren el = elChildren $ head $ tail $ onlyElems el

attr n el = fromJust $ findAttr (QName n Nothing Nothing) el
 
filterElems name = filter (\e -> qName (elName e) == name)


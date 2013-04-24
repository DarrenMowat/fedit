module FOUL.FOULLang where

import FOUL.Language
import FOUL.Parser

parseAndEval :: String -> Either String String
parseAndEval []    = Left "Empty Input"
parseAndEval input = Left "Not Implemented"
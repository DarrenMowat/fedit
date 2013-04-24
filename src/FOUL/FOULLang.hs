module FOUL.FOULLang where

import FOUL.Language
import FOUL.Parser

parseAndEval :: String -> Either String Prog
parseAndEval []    = Left "Empty Input"
parseAndEval input = parseProgram input
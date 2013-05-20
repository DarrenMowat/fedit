module Main where

{-# LANGUAGE ForeignFunctionInterface #-}

import Data.List
import Foreign
import Foreign.C (CInt(..))
import System.IO
import System.Directory
import System.Environment

import Editor.ANSIEscapes
import Editor.Block
import Editor.Overlay
import Editor.KeyHandler
import FOUL.Blackbox
import FOUL.FOUL
import Util.LogUtils

data Window = Window
type WindowPtr = Ptr Window

horizBorder :: Int
horizBorder = 4

lineNumGutter :: Int
lineNumGutter = 4

foreign import ccall
  initscr :: IO () 

foreign import ccall "endwin"
  endwin :: IO CInt

foreign import ccall "refresh"
  refresh :: IO CInt

foreign import ccall "&LINES"
  linesPtr :: Ptr CInt

foreign import ccall "&COLS"
  colsPtr :: Ptr CInt

scrSize :: IO (Int, Int)
scrSize = do
    lnes <- peek linesPtr
    cols <- peek colsPtr
    return ((fromIntegral cols), fromIntegral lnes)

copies :: Int -> a -> [a]
copies n a = take n (repeat a)

crlf :: IO ()
crlf = putStr "\r\n"

putLn :: String -> IO ()
putLn x = putStr x >> crlf

type ScreenState = (Point, Size)
  -- position in buffer of top left corner of screen, screen size

onScreen :: Point -> ScreenState -> ScreenState
onScreen (cx, cy) ((px, py), s@(sw, sh))
  = (( intoRange px cx sw, intoRange py cy sh), (sw, sh - 1))
  where
    intoRange i j x
      | i <= j && j <= i + x = i   -- in range, no change
      | otherwise = max 0 (j - div x 2)

getEscapeKey :: [(String, Key)] -> IO (Maybe Key)
getEscapeKey [] = return Nothing
getEscapeKey sks = case lookup "" sks of
  Just k -> return (Just k)
  _ -> do
    c <- getChar
    getEscapeKey [(cs, k) | (d : cs, k) <- sks, d == c]

keyReady :: IO (Maybe Key)
keyReady = do
  -- b <- hReady stdin -- This returns false most of the time so this function is constantly looping
  -- Pins my CPU high, use hWaitForInput instead and wait forever by passing -1 ;)
  b <- hWaitForInput stdin (-1)
  if not b then return Nothing else do
    c <- getChar
    case (shout (show c) c) of
      '\n' -> return $ Just Return
      '\r' -> return $ Just Return
      '\b' -> return $ Just Backspace
      '\DEL' -> return $ Just Backspace
      _ | c >= ' ' -> return $ Just (CharKey c)
      '\ENQ' -> return $ Just Eval -- Eval
      '\ESC' -> do
        b <- hReady stdin
        if not b then return $ Just Quit else do
          c <- getChar
          case c of
            '[' -> getEscapeKey escapeKeys
            _ -> return $ Just Quit
      _ -> return $ Nothing

outer :: ScreenState -> EditorContext -> IO ()
outer ps (EC tc f) = inner ps tc (whatAndWhere tc) LotsChanged
  where
  inner ps@(p, s) tc@(czz, cur, css) lc@(l, c@(cx, cy)) d = do
    refresh
    s' <- scrSize
    let ps'@((px, py), (sw, _)) = onScreen c (p, s')
    let d' = if ps /= ps' then LotsChanged else d
    case d' of
      LotsChanged -> do
        shout "LOTS CHANGED" clearScreen
        resetCursor
        mapM_ putStr $ layout $ cropLay cropBox ps' l
      LineChanged -> do
        resetCursor
        down (cy - py)
        mapM_ putStr $ layout $ cropLay cropBox ((px, cy), (sw, 1)) l
      _ -> return ()
    if d' > NoChange then do
      resetCursor
      forward (cx - px)
      down (cy - py)
     else return ()
    mc <- keyReady
    case mc of
      Nothing -> inner ps' tc lc NoChange
      Just Quit -> do 
        case (fst f) of 
          "" -> return ()
          p  -> do 
            let ls = textCursorToText tc
            writeFile p (unlines ls)
            res <- evaluateContents p
            endwin
            putStrLn res
      Just Eval -> do 
        let (x, cs) = deactivate cur
        let (y, strs) = deactivate (czz, Here, cs : css)
        nb <- runBlackbox (fst f) (unlines strs)
        let (as, (b:bs)) = splitAt (y - 1) $ lines nb
        inner ps (toBwdList as, (B0, Here, b), bs) (whatAndWhere tc) LotsChanged
      Just k -> case handleKey k tc of
        Nothing -> inner ps' tc lc NoChange
        Just (d, tc') -> inner ps' tc' (whatAndWhere tc') d

evaluateContents :: FilePath -> IO String
evaluateContents file = do
  res <- parseToFoulFromFile file
  case res of
    Left err -> return $ "ParserError: " ++ (show err)
    Right prog -> do 
      let ev = evalMain prog
      case ev of 
        Left err -> return $ "EvalError: " ++ (show err)
        Right va -> return $ "\nmain() -> " ++ (show va) ++ "\n"

main :: IO ()
main = do 
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  xs <- getArgs
  econt <- case xs of
    [] -> return ("", "")
    (x : _) -> do 
      exists <- doesFileExist x
      case exists of 
        True -> do 
          contents <- readFile x
          return (x, contents)
        False -> return (x, "")
  let (l, ls) = case lines (snd econt) of
        [] -> ("", [])
        (l : ls) -> (l, ls)
  initscr
  outer ((0, 0), (-1, -1)) (EC (mkTextCursor l ls) econt)
  return ()

mkTextCursor :: String -> [String] -> TextCursor 
mkTextCursor s ss = (B0, (B0, Here, s), ss)

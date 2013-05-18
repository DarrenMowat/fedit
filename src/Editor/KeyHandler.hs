
module Editor.KeyHandler where

import Editor.Block
import Editor.Overlay

import Util.LogUtils

{- From the lecture, here's that type of backward lists. -}

data Bwd x = B0 | Bwd x :< x deriving (Show, Eq)

{- A slight improvement on the lecture: this is a cursor with x things
   round the outside, and an m in the middle. The idea is that we keep
   everything in exactly the right order, so you can always see what's
   where. -}

type Cursor x m = (Bwd x, m, [x])

{- Here's something to put in the middle, to show where you are. -}

data Here = Here deriving Show

{- If you start working with selections, you may wish to modify the
   Here type to account for the current state, e.g. {no selection,
   selection left of cursor, or selection right of cursor}.      -}

{- For one line, we have characters either side, and Here in the middle. -}

type StringCursor = Cursor Char Here

{- For multiple lines, we have strings above and below, and a StringCursor
   in the middle. -}

type TextCursor = Cursor String StringCursor

type EditorFile = (FilePath, String)

data EditorContext = EC TextCursor EditorFile deriving (Show)

{- Useful equipment: deactivate turns a cursor into a list by shuffling
   everything to the right, but it also tells you /numerically/ where the
   cursor was. This might help you implement up and down, for example. -}

deactivate :: Cursor x Here -> (Int, [x])
deactivate c = outward 0 c where
  outward i (B0, Here, xs)       = (i, xs)
  outward i (xz :< x, Here, xs)  = outward (i + 1) (xz, Here, x : xs)

{- Activate turns a list into a cursor open at the given position, or as
   near as it gets. -}

activate :: (Int, [x]) -> Cursor x Here
activate (i, xs) = inward i (B0, Here, xs) where
  inward _ c@(_, Here, [])     = c  -- we can go no further
  inward 0 c                   = c  -- we should go no further
  inward i (xz, Here, x : xs)  = inward (i - 1) (xz :< x, Here, xs)  -- and on!

{- Now, if you give me a TextCursor, I can compute the corresponding
   Layout Box, together with the coordinates of Here.
   This is how my code figures out what to display and where to put the
   cursor. -}

lineNumGutterSize :: Int
lineNumGutterSize = 6

whatAndWhere :: TextCursor -> (Layout Box, Point)
whatAndWhere (czz, cur, css) = (foldr (joinV . layS) layZ strs, (x, y)) where
  (x, cs) = deactivate cur
  (y, strs) = deactivate (czz, Here, cs : css)

{- Next, you'll need some model of keystrokes. Here's a type describing
   some keystrokes. You may want more. -}

data ArrowDir = UpArrow | DownArrow | LeftArrow | RightArrow
data Modifier = Normal | Shift | Control

data Key
  = CharKey Char                -- an ordinary printable character
  | ArrowKey Modifier ArrowDir  -- an arrow key
  | Return
  | Backspace
  | Delete
  | Quit

{- Keys come in as standard ANSI escape sequences. You can look 'em up
   online. Feel free to extend escapeKeys so that more keystrokes get
   translated. -}

directions :: [(Char, ArrowDir)]
directions = [('A', UpArrow), ('B', DownArrow),
              ('C', RightArrow), ('D', LeftArrow)]

escapeKeys :: [(String, Key)]
escapeKeys =
  [([c], ArrowKey Normal d) | (c, d) <- directions] ++
  [("1;2" ++ [c], ArrowKey Shift d) | (c, d) <- directions] ++
  [("1;5" ++ [c], ArrowKey Control d) | (c, d) <- directions] ++
  [("3~", Delete)]

{- Last but not least, you get to tell my code how much damage you've done.
   This makes the redrawing more efficient: if you've done less damage to
   the file, my code needs to do less to update. If in doubt, overestimate
   the damage: a slow display is better than a broken display. -}

data Damage
  = NoChange       -- use this if nothing at all happened
  | PointChanged   -- use this if you moved the cursor but kept the text
  | LineChanged    -- use this if you changed text only on the current line
  | LotsChanged    -- use this if you changed text off the current line
  deriving (Show, Eq, Ord)

{--------------------------------------------------------------------------}
{- AT LAST, YOUR BIT!                                                     -}
{- Given a Key and an initial TextCursor, either reject the keystroke or  -}
{- return a modified cursor, with an overestimate of the damage you've    -}
{- you've done. To give you the idea, I've supplied a broken version of   -}
{- ordinary typing, which you get to fix.                                 -}
{-                                                                        -}
{- Be creative!                                                           -}
{--------------------------------------------------------------------------}

{--------------------------------------------------------------------------}
{- Things Completed                                                       -}
{-                                                                        -}
{- Improve Performance in Main.hs - No longer loop constantly til stdin   -}
{- is ready. Block & wait for it to be ready                              -}
{- Handle typing                                                          -}
{- Handle Return                                                          -}
{- Handle Backspace                                                       -}
{- Handle Arrow Keys                                                      -}
{-                                                                        -}
{--------------------------------------------------------------------------}

handleKey :: Key -> TextCursor -> Maybe (Damage, TextCursor)
handleKey (ArrowKey mod dir) x =  handleArrowKey (ArrowKey mod dir) x
handleKey Return (sz, (cz, Here, cs), ss) 
               = Just (LotsChanged, (sz :< stringCursorToString(cz, Here, []), (B0, Here, cs), ss))
handleKey Backspace x = handleBackspace (x)
handleKey (CharKey c) (sz, (cz, Here, cs), ss)
               = Just (LineChanged, (sz, (cz :< c, Here, cs), ss))
handleKey _ x = Just (NoChange, x)


{- 
  Function to handle what happens to a TextCurosr when the backspace is pressed

  Done!
-}
handleBackspace :: TextCursor -> Maybe (Damage, TextCursor)
{- Backspace on an empty editor - Do Nothing -}
handleBackspace (B0, (B0 , Here, cs), ns) 
               = Just (NoChange, (B0, (B0 , Here, cs), ns))
{- Backspace on an empty line - Move to line above. Delete no characters. -}
handleBackspace (bs :< b, (B0 , Here, cs), ns) 
               = Just (LotsChanged, (bs, activate((length b), b ++ cs), ns))
{- Backspace presses on a line which is not empty. Delete a character -}
handleBackspace (sz, (cz :< c, Here, cs), ss) 
               = Just (LineChanged, (sz, (cz, Here, cs), ss))

{- 
  Function to handle what happens to a TextCursor when an arrow key is pressed 

  Done!
-}
handleArrowKey :: Key -> TextCursor -> Maybe (Damage, TextCursor)
{- Left arrow key at the start of the editor - Do Nothing -}
handleArrowKey (ArrowKey _ LeftArrow) (B0, (B0, Here, cs), as) 
                = Just (NoChange, (B0, (B0, Here, cs), as))
{- Left arrow key at the start of a line - Move to the end of the last line -}
handleArrowKey (ArrowKey _ LeftArrow) (bs :< b, (B0, Here, cs), as) 
                = Just (LotsChanged, (bs, activate ((length b), b), cs:as))
{- Left Arrow Key on non empty line - Move cursor one place to the left -}
handleArrowKey (ArrowKey _ LeftArrow) (bs, (cz :< c, Here, cs), as) 
                = Just (LineChanged, (bs, (cz, Here, c:cs), as))
{- Right arrow key at the end of the editor - Do Nothing -}
handleArrowKey (ArrowKey _ RightArrow) (bs, (b, Here, []), []) 
                = Just (NoChange, (bs, (b, Here, []), []))
{- Right arrow key at the end of a line - Move to the start of the next line -}
handleArrowKey (ArrowKey _ RightArrow) (bs, (b, Here, []), a:as) 
                = Just (LotsChanged, (bs :< (stringCursorToString (b, Here, [])), activate (0, a), as))
{- Right Arrow Key on non empty line - Move cursor one place to the right -}
handleArrowKey (ArrowKey _ RightArrow) (bs, (cz, Here, c:cs), as) 
                = Just (LineChanged, (bs, (cz :< c, Here, cs), as))  
{- Up arrow key when there is no content above - Do nothing -}   
handleArrowKey (ArrowKey _ UpArrow) (B0, cs, as) 
                = Just (NoChange, (B0, cs, as))
{- Up arrow key - Move up a line & attempt to remember the current line position -}   
handleArrowKey (ArrowKey _ UpArrow) (bs :< b, cs, as) 
                = Just (LotsChanged, (bs, activate (di, b), ds:as)) 
                  where (di, ds) = deactivate cs
{- Down arrow key when there is no content below - Do nothing -}   
handleArrowKey (ArrowKey _ DownArrow) (bs, cs, []) 
                = Just (NoChange, (bs, cs, []))
{- Down arrow key - Move down a line & attempt to remember the current line position -}  
handleArrowKey (ArrowKey _ DownArrow) (bs, cs, a:as) 
                = Just (LotsChanged, (bs :< ds, activate (di, a), as))
                  where (di, ds) = deactivate cs

{- 
  Utility function which returns a string representaion from the given StringCursor
  when we don't care about the current cursor position
-}
stringCursorToString :: StringCursor -> String
stringCursorToString s = snd $ deactivate s

textCursorToText :: TextCursor -> [String]
textCursorToText (bs, c, xs) = toFwd bs ++ stringCursorToString c : xs

toFwd :: Bwd a -> [a]
toFwd B0        = []
toFwd (bs :< b) = toFwd bs ++ [b] 

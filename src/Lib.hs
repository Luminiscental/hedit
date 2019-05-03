module Lib
    ( start
    )
where

import           System.Environment
import           System.Directory
import           Control.Lens
import           Data.List
import           Data.Maybe
import           Graphics.Vty

-- Pure utility functions
stableLines :: String -> [String]
stableLines = ((++) <$> init <*> (: []) . init . last) . lines . (++ "%")

stableUnlines :: [String] -> String
stableUnlines = foldl1 (\acc line -> acc ++ '\n' : line)

-- Vty utility functions
textAttr :: Attr
textAttr = defAttr `withForeColor` blue `withBackColor` black

cursorAttr :: Attr
cursorAttr = defAttr `withForeColor` green `withBackColor` black

cursorImg :: Image
cursorImg = string cursorAttr "|"

textToImg :: String -> Image
textToImg = string textAttr

strToImgs :: String -> [Image]
strToImgs = map textToImg . stableLines

stackImgs :: [Image] -> Image
stackImgs = foldl (<->) emptyImage

-- EditState data type
data EditMode = NormalMode | InsertMode;
data EditFile = ExistingFile String | NewFile;

data EditState = EditState
  { beforeCursor :: String
  , afterCursor :: String
  , editMode :: EditMode
  , editFile :: EditFile
  }

-- EditState constructors
emptyEditState :: EditState
emptyEditState = EditState { beforeCursor = ""
                           , afterCursor  = ""
                           , editMode     = NormalMode
                           , editFile     = NewFile
                           }

editString :: String -> EditState
editString str = EditState { beforeCursor = ""
                           , afterCursor  = str
                           , editMode     = NormalMode
                           , editFile     = ExistingFile str
                           }

-- EditState properties
getText :: EditState -> String
getText EditState { beforeCursor = b, afterCursor = a } = reverse b ++ a

row :: EditState -> Int
row = subtract 1 . length . stableLines . beforeCursor

column :: EditState -> Int
column = (fromMaybe <$> length <*> elemIndex '\n') . beforeCursor

nextLine :: EditState -> Maybe (Int, Int)
nextLine editState = do
    let after   = afterCursor editState
        indices = elemIndices '\n' after
    preIdx <- indices ^? element 0
    let endIdx = fromMaybe (length after) $ indices ^? element 1
    return (preIdx + 1, endIdx)

showPos :: EditState -> String
showPos = combineToString <$> row <*> column
    where combineToString row col = show row ++ ":" ++ show col

renderText :: EditState -> Image
renderText = stackImgs . strToImgs . getText

-- EditState mutations

setEditFile :: FilePath -> EditState -> EditState
setEditFile path EditState { beforeCursor = bs, afterCursor = as, editMode = mode, editFile = _ }
    = EditState { beforeCursor = bs
                , afterCursor  = as
                , editMode     = mode
                , editFile     = ExistingFile path
                }

setEditMode :: EditMode -> EditState -> EditState
setEditMode mode EditState { beforeCursor = bs, afterCursor = as, editMode = _, editFile = editFile }
    = EditState { beforeCursor = bs
                , afterCursor  = as
                , editMode     = mode
                , editFile     = editFile
                }

flipAroundCursor :: EditState -> EditState
flipAroundCursor EditState { beforeCursor = bs, afterCursor = as, editMode = editMode, editFile = editFile }
    = EditState { beforeCursor = as
                , afterCursor  = bs
                , editMode     = editMode
                , editFile     = editFile
                }

flipAroundRow :: EditState -> EditState
flipAroundRow editState =
    let beforeLines = stableLines . beforeCursor $ editState
        afterLines  = stableLines . afterCursor $ editState
        flipped ls = map reverse $ tail ls
        flippedToAfter  = flipped beforeLines
        flippedToBefore = flipped afterLines
        stillBefore     = head beforeLines
        stillAfter      = head afterLines
    in  EditState { beforeCursor = stableUnlines $ stillBefore : flippedToBefore
                  , afterCursor  = stableUnlines $ stillAfter : flippedToAfter
                  , editMode     = editMode editState
                  , editFile     = editFile editState
                  }

moveRight :: EditState -> EditState
moveRight EditState { beforeCursor = bs, afterCursor = a : as, editMode = editMode, editFile = editFile }
    = EditState { beforeCursor = a : bs
                , afterCursor  = as
                , editMode     = editMode
                , editFile     = editFile
                }
moveRight s = s

moveLeft :: EditState -> EditState
moveLeft = flipAroundCursor . moveRight . flipAroundCursor

moveDown :: EditState -> EditState
moveDown editState@EditState { beforeCursor = bs, afterCursor = as, editMode = editMode, editFile = editFile }
    = let col    = column editState
          offset = fromMaybe 0 $ do
              (startIdx, endIdx) <- nextLine editState
              return $ if col <= endIdx - startIdx
                  then startIdx + col
                  else endIdx
          (skipped, left) = splitAt offset as
      in  EditState { beforeCursor = reverse skipped ++ bs
                    , afterCursor  = left
                    , editMode     = editMode
                    , editFile     = editFile
                    }

moveUp :: EditState -> EditState
moveUp = flipAroundRow . moveDown . flipAroundRow

pushEdit :: Char -> EditState -> EditState
pushEdit c EditState { beforeCursor = bs, afterCursor = as, editMode = editMode, editFile = editFile }
    = EditState { beforeCursor = c : bs
                , afterCursor  = as
                , editMode     = editMode
                , editFile     = editFile
                }

pushEdits :: String -> EditState -> EditState
pushEdits str editState = foldl (flip pushEdit) editState str

popEdit :: EditState -> EditState
popEdit EditState { beforeCursor = b : bs, afterCursor = as, editMode = editMode, editFile = editFile }
    = EditState { beforeCursor = bs
                , afterCursor  = as
                , editMode     = editMode
                , editFile     = editFile
                }
popEdit s = s

popEdits :: Int -> EditState -> EditState
popEdits 0 editState = editState
popEdits n editState = popEdits (n - 1) $ popEdit editState

popLine :: EditState -> EditState
popLine editState =
    let before   = beforeCursor editState
        after    = afterCursor editState
        startIdx = fromMaybe (-1) $ elemIndex '\n' before
        endIdx   = fromMaybe (-1) $ elemIndex '\n' after
    in  EditState { beforeCursor = drop (startIdx + 1) before
                  , afterCursor  = drop endIdx after
                  , editMode     = editMode editState
                  , editFile     = editFile editState
                  }

handleNormalEvent :: EditState -> Event -> (Bool, EditState)
handleNormalEvent editState event = case event of
    EvKey KEsc        [] -> (True, editState)
    EvKey (KChar 'i') [] -> (False, setEditMode InsertMode editState)
    EvKey KLeft       [] -> (False, moveLeft editState)
    EvKey KRight      [] -> (False, moveRight editState)
    EvKey KDown       [] -> (False, moveDown editState)
    EvKey KUp         [] -> (False, moveUp editState)
    EvKey (KChar 'h') [] -> (False, moveLeft editState)
    EvKey (KChar 'j') [] -> (False, moveDown editState)
    EvKey (KChar 'k') [] -> (False, moveUp editState)
    EvKey (KChar 'l') [] -> (False, moveRight editState)
    EvKey (KChar 'x') [] -> (False, popEdit editState)
    EvKey (KChar 'd') [] -> (False, popLine editState)
    EvKey (KChar 'c') [] ->
        (False, setEditMode InsertMode . popEdit $ editState)
    _ -> (False, editState)

handleInsertEvent :: EditState -> Event -> (Bool, EditState)
handleInsertEvent editState event = case event of
    EvKey KEsc         [] -> (False, setEditMode NormalMode editState)
    EvKey KLeft        [] -> (False, moveLeft editState)
    EvKey KRight       [] -> (False, moveRight editState)
    EvKey KDown        [] -> (False, moveDown editState)
    EvKey KUp          [] -> (False, moveUp editState)
    EvKey KEnter       [] -> (False, pushEdit '\n' editState)
    EvKey KBS          [] -> (False, popEdit editState)
    EvKey (KChar '\t') [] -> (False, pushEdits "    " editState)
    EvKey (KChar c   ) [] -> (False, pushEdit c editState)
    _                     -> (False, editState)

-- IO Functions
loadFile :: FilePath -> IO EditState
loadFile filename = do
    fileExists <- doesFileExist filename
    contents   <- if fileExists then readFile filename else return ""
    return . setEditFile filename . editString $ contents

start :: IO ()
start = do
    cfg       <- standardIOConfig
    vty       <- mkVty cfg
    args      <- getArgs
    editState <- case args ^? element 0 of
        Just name -> loadFile name
        Nothing   -> return emptyEditState
    run vty editState
    shutdown vty

run :: Vty -> EditState -> IO ()
run vty editState = do
    render vty editState
    (shouldExit, editState) <- handleEvent vty editState
    if shouldExit then closeFile vty editState else run vty editState

askInput :: Vty -> String -> String -> IO String
askInput vty msg input = do
    let content      = msg ++ " " ++ input
        displayLines = stableLines content
        r            = subtract 1 . length $ displayLines
        c            = length . last $ displayLines
        img          = stackImgs . strToImgs $ content
        cursor       = Cursor c r
        pic          = Picture cursor [img] ClearBackground
    update vty pic
    event <- nextEvent vty
    case event of
        EvKey KEnter    [] -> return input
        EvKey KBS       [] -> askInput vty msg . init $ input
        EvKey (KChar c) [] -> askInput vty msg (input ++ [c])
        _                  -> askInput vty msg input

closeFile :: Vty -> EditState -> IO ()
closeFile vty editState = do
    filename <- askInput vty "Save file as:" $ case editFile editState of
        ExistingFile str -> str
        NewFile          -> ""
    writeFile filename . getText $ editState

handleEvent :: Vty -> EditState -> IO (Bool, EditState)
handleEvent vty editState = do
    event <- nextEvent vty
    case editMode editState of
        NormalMode -> return $ handleNormalEvent editState event
        InsertMode -> return $ handleInsertEvent editState event

render :: Vty -> EditState -> IO ()
render vty editState = do
    let img    = renderText editState
    let r      = row editState
    let c      = column editState
    let cursor = Cursor c r
    let pic    = Picture cursor [img] ClearBackground
    update vty pic

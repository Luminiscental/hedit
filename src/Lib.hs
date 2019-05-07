module Lib
    ( start
    , stableLines
    , stableUnlines
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

textToImg :: String -> Image
textToImg = string textAttr

strToImgs :: String -> [Image]
strToImgs = map textToImg . stableLines

stackImgs :: [Image] -> Image
stackImgs = foldl (<->) emptyImage

-- EditState data type

data EditableText = EditableText { beforeCursor :: String, afterCursor :: String }
data EditMode = NormalMode | InsertMode;
data EditFile = ExistingFile String | NewFile;

data EditState = EditState
  { editText :: EditableText
  , editMode :: EditMode
  , editFile :: EditFile
  , viewOffset :: Int
  }

-- EditState constructors

emptyText :: EditableText
emptyText = EditableText { beforeCursor = "", afterCursor = "" }

emptyEditState :: EditState
emptyEditState = EditState { editText   = emptyText
                           , editMode   = NormalMode
                           , editFile   = NewFile
                           , viewOffset = 0
                           }

editString :: String -> EditableText
editString str = EditableText { beforeCursor = "", afterCursor = str }

-- EditState properties

getText :: EditableText -> String
getText EditableText { beforeCursor = b, afterCursor = a } = reverse b ++ a

row :: EditableText -> Int
row = subtract 1 . length . stableLines . beforeCursor

column :: EditableText -> Int
column = (fromMaybe <$> length <*> elemIndex '\n') . beforeCursor

nextLine :: EditableText -> Maybe (Int, Int)
nextLine editState = do
    let after   = afterCursor editState
        indices = elemIndices '\n' after
        endIdx  = fromMaybe (length after) $ indices ^? element 1
    preIdx <- indices ^? element 0
    return (preIdx + 1, endIdx)

showPos :: EditableText -> String
showPos = combineToString <$> row <*> column
    where combineToString row col = show row ++ ":" ++ show col

linesBefore :: EditableText -> [String]
linesBefore = init . stableLines . reverse . beforeCursor

linesAfter :: EditableText -> [String]
linesAfter = tail . stableLines . afterCursor

cursorLine :: EditableText -> String
cursorLine =
    (++)
        <$> last
        .   stableLines
        .   reverse
        .   beforeCursor
        <*> head
        .   stableLines
        .   afterCursor

renderText :: Int -> Int -> EditableText -> Image
renderText offset height editText =
    let aboveLines  = linesBefore editText
        belowLines  = linesAfter editText
        centerLine  = cursorLine editText
        allLines    = aboveLines ++ centerLine : belowLines
        renderLines = take height . drop offset $ allLines
        imgs        = strToImgs . stableUnlines $ renderLines
    in  stackImgs imgs

-- EditState mutations

setEditText :: EditableText -> EditState -> EditState
setEditText text EditState { editText = _, editMode = m, editFile = f, viewOffset = o }
    = EditState { editText = text, editMode = m, editFile = f, viewOffset = o }

setEditFile :: FilePath -> EditState -> EditState
setEditFile path EditState { editText = t, editMode = m, editFile = _, viewOffset = o }
    = EditState { editText   = t
                , editMode   = m
                , editFile   = ExistingFile path
                , viewOffset = o
                }

setEditMode :: EditMode -> EditState -> EditState
setEditMode mode EditState { editText = t, editMode = _, editFile = f, viewOffset = o }
    = EditState { editText = t, editMode = mode, editFile = f, viewOffset = o }

setViewOffset :: Int -> EditState -> EditState
setViewOffset offset EditState { editText = t, editFile = f, editMode = m, viewOffset = _ }
    = EditState { editText   = t
                , editFile   = f
                , editMode   = m
                , viewOffset = offset
                }

flipAroundCursor :: EditableText -> EditableText
flipAroundCursor EditableText { beforeCursor = bs, afterCursor = as } =
    EditableText { beforeCursor = as, afterCursor = bs }

flipAroundRow :: EditableText -> EditableText
flipAroundRow editState =
    let beforeLines     = stableLines . beforeCursor $ editState
        afterLines      = stableLines . afterCursor $ editState
        flipped         = map reverse . tail
        flippedToAfter  = flipped beforeLines
        flippedToBefore = flipped afterLines
        stillBefore     = head beforeLines
        stillAfter      = head afterLines
    in  EditableText
            { beforeCursor = stableUnlines $ stillBefore : flippedToBefore
            , afterCursor  = stableUnlines $ stillAfter : flippedToAfter
            }

moveRight :: EditableText -> EditableText
moveRight EditableText { beforeCursor = bs, afterCursor = a : as } =
    EditableText { beforeCursor = a : bs, afterCursor = as }
moveRight t = t

moveLeft :: EditableText -> EditableText
moveLeft = flipAroundCursor . moveRight . flipAroundCursor

moveDown :: EditableText -> EditableText
moveDown editText =
    let col    = column editText
        offset = fromMaybe 0 $ do
            (startIdx, endIdx) <- nextLine editText
            return $ if col <= endIdx - startIdx then startIdx + col else endIdx
        (skipped, left) = splitAt offset $ afterCursor editText
    in  EditableText { beforeCursor = reverse skipped ++ beforeCursor editText
                     , afterCursor  = left
                     }

moveUp :: EditableText -> EditableText
moveUp = flipAroundRow . moveDown . flipAroundRow

pushEdit :: Char -> EditableText -> EditableText
pushEdit c EditableText { beforeCursor = bs, afterCursor = as } =
    EditableText { beforeCursor = c : bs, afterCursor = as }

pushEdits :: String -> EditableText -> EditableText
pushEdits str editText = foldl (flip pushEdit) editText str

popEdit :: EditableText -> EditableText
popEdit EditableText { beforeCursor = b : bs, afterCursor = as } =
    EditableText { beforeCursor = bs, afterCursor = as }
popEdit t = t

popEdits :: Int -> EditableText -> EditableText
popEdits 0 = id
popEdits n = popEdits (n - 1) . popEdit

popLine :: EditableText -> EditableText
popLine editText =
    let before   = beforeCursor editText
        after    = afterCursor editText
        startIdx = fromMaybe (-1) $ elemIndex '\n' before
        endIdx   = fromMaybe (-1) $ elemIndex '\n' after
    in  EditableText { beforeCursor = drop (startIdx + 1) before
                     , afterCursor  = drop endIdx after
                     }

updateOffset :: Int -> EditState -> EditState
updateOffset height editState = setViewOffset newOffset editState  where
    newOffset =
        let r = row . editText $ editState
            o = viewOffset editState
        in  if r - o < 0 then r else if r - o >= height then r - height else o


applyEdit :: Int -> (EditableText -> EditableText) -> EditState -> EditState
applyEdit height edit state = updateOffset
    height
    EditState { editText   = edit . editText $ state
              , editMode   = editMode state
              , editFile   = editFile state
              , viewOffset = viewOffset state
              }

handleNormalEvent :: Int -> EditState -> Event -> (Bool, EditState)
handleNormalEvent height editState event = case event of
    EvKey KEsc        [] -> (True, editState)
    EvKey (KChar 'i') [] -> (False, setEditMode InsertMode editState)
    EvKey KLeft       [] -> (False, applyEdit height moveLeft editState)
    EvKey KRight      [] -> (False, applyEdit height moveRight editState)
    EvKey KDown       [] -> (False, applyEdit height moveDown editState)
    EvKey KUp         [] -> (False, applyEdit height moveUp editState)
    EvKey (KChar 'h') [] -> (False, applyEdit height moveLeft editState)
    EvKey (KChar 'j') [] -> (False, applyEdit height moveDown editState)
    EvKey (KChar 'k') [] -> (False, applyEdit height moveUp editState)
    EvKey (KChar 'l') [] -> (False, applyEdit height moveRight editState)
    EvKey (KChar 'x') [] -> (False, applyEdit height popEdit editState)
    EvKey (KChar 'd') [] -> (False, applyEdit height popLine editState)
    EvKey (KChar 'c') [] ->
        (False, setEditMode InsertMode . applyEdit height popEdit $ editState)
    _ -> (False, editState)

handleInsertEvent :: Int -> EditState -> Event -> (Bool, EditState)
handleInsertEvent height editState event = case event of
    EvKey KEsc   [] -> (False, setEditMode NormalMode editState)
    EvKey KLeft  [] -> (False, applyEdit height moveLeft editState)
    EvKey KRight [] -> (False, applyEdit height moveRight editState)
    EvKey KDown  [] -> (False, applyEdit height moveDown editState)
    EvKey KUp    [] -> (False, applyEdit height moveUp editState)
    EvKey KEnter [] -> (False, applyEdit height (pushEdit '\n') editState)
    EvKey KBS    [] -> (False, applyEdit height popEdit editState)
    EvKey (KChar '\t') [] ->
        (False, applyEdit height (pushEdits "    ") editState)
    EvKey (KChar c) [] -> (False, applyEdit height (pushEdit c) editState)
    _                  -> (False, editState)

-- IO Functions

loadFile :: FilePath -> IO EditState
loadFile filename = do
    fileExists <- doesFileExist filename
    contents   <- if fileExists then readFile filename else return ""
    return
        . setEditFile filename
        . ($ emptyEditState)
        . setEditText
        . editString
        $ contents

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
    filename <-
        askInput vty "Saving...\n\n  save file as:" $ case editFile editState of
            ExistingFile str -> str
            NewFile          -> ""
    writeFile filename . getText . editText $ editState

handleEvent :: Vty -> EditState -> IO (Bool, EditState)
handleEvent vty editState = do
    event <- nextEvent vty
    let output = outputIface vty
    (width, height) <- displayBounds output
    case editMode editState of
        NormalMode -> return $ handleNormalEvent height editState event
        InsertMode -> return $ handleInsertEvent height editState event

render :: Vty -> EditState -> IO ()
render vty editState = do
    let output = outputIface vty
    (width, height) <- displayBounds output
    let offset = viewOffset editState
        img    = renderText offset height . editText $ editState
        r      = row . editText $ editState
        c      = column . editText $ editState
        cursor = Cursor c (r - offset)
        pic    = Picture cursor [img] ClearBackground
    update vty pic

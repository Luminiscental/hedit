{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Lib
    ( start
    , stableLines
    , stableUnlines
    )
where

import           System.Environment
import           System.Directory
import           Control.Lens
import           Control.Monad
import           Data.List
import qualified Data.Text                     as T
import qualified Data.Map.Lazy                 as M
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

-- Types

data EditableText = EditableText { beforeCursor :: String, afterCursor :: String }
data EditMode = NormalMode | InsertMode | ExitMode deriving (Eq, Ord);
data EditFile = ExistingFile String | NewFile;

data EditState = EditState
  { editText :: EditableText
  , editMode :: EditMode
  , editFile :: EditFile
  , viewOffset :: Int
  }

type MetaEdit = EditState -> EditState
type SimpleEdit = EditableText -> EditableText

type MetaBindings = M.Map Key MetaEdit
type SimpleBindings = M.Map Key SimpleEdit
type FallbackBinding = Key -> MetaEdit

type BindingMap = M.Map EditMode (MetaBindings, FallbackBinding)

-- Constructors / Initializers

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

-- Properties / Accessors

shouldExit :: EditState -> Bool
shouldExit editState = case editMode editState of
    ExitMode -> True
    _        -> False

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

-- Edits / Transformations

setEditText :: EditableText -> MetaEdit
setEditText text EditState { editText = _, editMode = m, editFile = f, viewOffset = o }
    = EditState { editText = text, editMode = m, editFile = f, viewOffset = o }

setEditFile :: FilePath -> MetaEdit
setEditFile path EditState { editText = t, editMode = m, editFile = _, viewOffset = o }
    = EditState { editText   = t
                , editMode   = m
                , editFile   = ExistingFile path
                , viewOffset = o
                }

setEditMode :: EditMode -> MetaEdit
setEditMode mode EditState { editText = t, editMode = _, editFile = f, viewOffset = o }
    = EditState { editText = t, editMode = mode, editFile = f, viewOffset = o }

setViewOffset :: Int -> MetaEdit
setViewOffset offset EditState { editText = t, editFile = f, editMode = m, viewOffset = _ }
    = EditState { editText   = t
                , editFile   = f
                , editMode   = m
                , viewOffset = offset
                }

flipAroundCursor :: SimpleEdit
flipAroundCursor EditableText { beforeCursor = bs, afterCursor = as } =
    EditableText { beforeCursor = as, afterCursor = bs }

flipAroundRow :: SimpleEdit
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

moveRight :: SimpleEdit
moveRight EditableText { beforeCursor = bs, afterCursor = a : as } =
    EditableText { beforeCursor = a : bs, afterCursor = as }
moveRight t = t

moveLeft :: SimpleEdit
moveLeft = flipAroundCursor . moveRight . flipAroundCursor

moveDown :: SimpleEdit
moveDown editText =
    let col    = column editText
        offset = fromMaybe 0 $ do
            (startIdx, endIdx) <- nextLine editText
            return $ if col <= endIdx - startIdx then startIdx + col else endIdx
        (skipped, left) = splitAt offset $ afterCursor editText
    in  EditableText { beforeCursor = reverse skipped ++ beforeCursor editText
                     , afterCursor  = left
                     }

moveUp :: SimpleEdit
moveUp = flipAroundRow . moveDown . flipAroundRow

pushEdit :: Char -> SimpleEdit
pushEdit c EditableText { beforeCursor = bs, afterCursor = as } =
    EditableText { beforeCursor = c : bs, afterCursor = as }

pushEdits :: String -> SimpleEdit
pushEdits str editText = foldl (flip pushEdit) editText str

popEdit :: SimpleEdit
popEdit EditableText { beforeCursor = b : bs, afterCursor = as } =
    EditableText { beforeCursor = bs, afterCursor = as }
popEdit t = t

popEdits :: Int -> SimpleEdit
popEdits 0 = id
popEdits n = popEdits (n - 1) . popEdit

popLine :: SimpleEdit
popLine editText =
    let before   = beforeCursor editText
        after    = afterCursor editText
        startIdx = fromMaybe (-1) $ elemIndex '\n' before
        endIdx   = fromMaybe (-1) $ elemIndex '\n' after
    in  EditableText { beforeCursor = drop (startIdx + 1) before
                     , afterCursor  = drop endIdx after
                     }

updateOffset :: Int -> MetaEdit
updateOffset height editState = setViewOffset newOffset editState  where
    newOffset =
        let r = row . editText $ editState
            o = viewOffset editState
        in  if r - o < 0 then r else if r - o >= height then r - height else o


applyEdit :: Int -> SimpleEdit -> MetaEdit
applyEdit height edit state = updateOffset
    height
    EditState { editText   = edit . editText $ state
              , editMode   = editMode state
              , editFile   = editFile state
              , viewOffset = viewOffset state
              }

-- Keybinds

makeBindings :: Int -> MetaBindings -> SimpleBindings -> MetaBindings
makeBindings height metaBindings editBindings =
    metaBindings <> M.map (applyEdit height) editBindings

bindings :: Int -> BindingMap
bindings height = M.fromList
    [ ( NormalMode
      , ( makeBindings
            height
            (M.fromList
                [ (KEsc     , setEditMode ExitMode)
                , (KChar 'i', setEditMode InsertMode)
                , (KChar 'c', setEditMode InsertMode . applyEdit height popEdit)
                ]
            )
            (M.fromList
                [ (KChar 'h', moveLeft)
                , (KChar 'j', moveDown)
                , (KChar 'k', moveUp)
                , (KChar 'l', moveRight)
                , (KChar 'x', popEdit)
                , (KChar 'd', popLine)
                , (KLeft    , moveLeft)
                , (KRight   , moveRight)
                , (KDown    , moveDown)
                , (KUp      , moveUp)
                ]
            )
        , const id
        )
      )
    , ( InsertMode
      , ( makeBindings
            height
            (M.fromList [(KEsc, setEditMode NormalMode)])
            (M.fromList
                [ (KLeft     , moveLeft)
                , (KRight    , moveRight)
                , (KDown     , moveDown)
                , (KUp       , moveUp)
                , (KEnter    , pushEdit '\n')
                , (KBS       , popEdit)
                , (KChar '\t', pushEdits "    ")
                ]
            )
        , \case
            KChar c -> applyEdit height (pushEdit c)
            _       -> id
        )
      )
    ]

lookupKeybind :: EditMode -> Key -> Int -> MetaEdit
lookupKeybind mode key height = fromMaybe
    id
    (do
        (modeBindings, fallback) <- M.lookup mode $ bindings height
        return $ fromMaybe (fallback key) (M.lookup key modeBindings)
    )

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
    editState <- handleEvent vty editState
    if shouldExit editState then closeFile vty editState else run vty editState

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

askYesNo :: Vty -> String -> IO Bool
askYesNo vty msg = do
    response <- askInput vty msg ""
    let firstChar = T.head . T.toLower $ T.pack response
    if firstChar == 'n'
        then return False
        else if firstChar == 'y'
            then return True
            else do
                putStrLn "I don't know if that means yes or no..."
                askYesNo vty msg

closeFile :: Vty -> EditState -> IO ()
closeFile vty editState = do
    shouldSave <- askYesNo vty "Do you want to save? [y/n]:"
    when shouldSave $ do
        filename <-
            askInput vty "Saving...\n\n  save file as:"
                $ case editFile editState of
                      ExistingFile str -> str
                      NewFile          -> ""
        writeFile filename . getText . editText $ editState

handleEvent :: Vty -> EditState -> IO EditState
handleEvent vty editState = do
    event <- nextEvent vty
    let output = outputIface vty
    (width, height) <- displayBounds output
    return
        (case event of
            EvKey key [] ->
                let mode = editMode editState
                in  lookupKeybind mode key height editState
            _ -> editState
        )

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

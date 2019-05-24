{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Lib
    ( loadEditState
    , runEditor
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
stableLines = actOnLast unmark . lines . markEnd
  where
    actOnLast f = (++) <$> init <*> makeList . f . last
    makeList = (: [])
    markEnd  = (++ "%")
    unmark   = init

stableUnlines :: [String] -> String
stableUnlines = foldl1 (\acc line -> acc ++ '\n' : line)

-- Vty utility functions

textAttr :: Attr
textAttr = defAttr `withForeColor` blue `withBackColor` black

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
shouldExit EditState { editMode = ExitMode } = True
shouldExit _ = False

getText :: EditableText -> String
getText = (++) <$> reverse . beforeCursor <*> afterCursor

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

splitBefore :: EditableText -> ([String], String)
splitBefore = splitAtLast . stableLines . reverse . beforeCursor
    where splitAtLast = (,) <$> init <*> last

splitAfter :: EditableText -> ([String], String)
splitAfter = splitAtFirst . stableLines . afterCursor
    where splitAtFirst = (,) <$> tail <*> head

linesBefore :: EditableText -> [String]
linesBefore = fst . splitBefore

linesAfter :: EditableText -> [String]
linesAfter = fst . splitAfter

cursorLine :: EditableText -> String
cursorLine = (++) <$> snd . splitBefore <*> snd . splitAfter

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
setEditText text state = state { editText = text }

setEditFile :: FilePath -> MetaEdit
setEditFile path state = state { editFile = ExistingFile path }

setEditMode :: EditMode -> MetaEdit
setEditMode mode state = state { editMode = mode }

setViewOffset :: Int -> MetaEdit
setViewOffset offset state = state { viewOffset = offset }

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
pushEdit c editText = editText { beforeCursor = c : beforeCursor editText }

pushEdits :: String -> SimpleEdit
pushEdits str editText = foldl (flip pushEdit) editText str

popEdit :: SimpleEdit
popEdit editText = editText { beforeCursor = drop 1 $ beforeCursor editText }

repeatApply :: (a -> a) -> Int -> (a -> a)
repeatApply f n = foldr (.) id (replicate n f)

popEdits :: Int -> SimpleEdit
popEdits = repeatApply popEdit

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
applyEdit height edit state =
    updateOffset height state { editText = edit . editText $ state }

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
        . flip setEditText emptyEditState
        . editString
        $ contents

loadEditState :: IO EditState
loadEditState = do
    args <- getArgs
    case args ^? element 0 of
        Just name -> loadFile name
        Nothing   -> return emptyEditState

runEditor :: Vty -> EditState -> IO ()
runEditor vty editState = do
    render vty editState
    editState <- handleEvent vty editState
    if shouldExit editState
        then closeFile vty editState
        else runEditor vty editState

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

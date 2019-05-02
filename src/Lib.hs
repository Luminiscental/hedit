module Lib
  ( start
  ) where

import Control.Lens
import Data.List
import Data.Maybe
import Graphics.Vty

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
textToImg = string textAttr . intersperse ' '

strToImgs :: String -> [Image]
strToImgs = map textToImg . stableLines

stackImgs :: [Image] -> Image
stackImgs = foldl (<->) emptyImage

-- EditState data type
data EditState = EditState
  { beforeCursor :: String
  , afterCursor :: String
  }

-- EditState constructors
emptyEditState :: EditState
emptyEditState = EditState {beforeCursor = "", afterCursor = ""}

-- EditState properties
row :: EditState -> Int
row = subtract 1 . length . stableLines . beforeCursor

column :: EditState -> Int
column = (fromMaybe <$> length <*> findIndex (== '\n')) . beforeCursor

nextLine :: EditState -> Maybe (Int, Int)
nextLine editState = do
  let after = afterCursor editState
      indices = findIndices (== '\n') after
  preIdx <- (indices ^? element 0)
  let endIdx = fromMaybe (length after - preIdx) $ indices ^? element 1
  return (preIdx + 1, endIdx)

showPos :: EditState -> String
showPos editState = combineToString <$> row <*> column $ editState
  where
    combineToString row col = show row ++ ":" ++ show col

renderEditState :: EditState -> Image
renderEditState editState =
  let imgsBefore = strToImgs $ reverse . beforeCursor $ editState
      imgsAfter = strToImgs . afterCursor $ editState
      imgMiddle = last imgsBefore <|> cursorImg <|> head imgsAfter
   in stackImgs
        [stackImgs (init imgsBefore), imgMiddle, stackImgs (tail imgsAfter)]

-- EditState mutations
flipAroundCursor :: EditState -> EditState
flipAroundCursor EditState {beforeCursor = bs, afterCursor = as} =
  EditState {beforeCursor = as, afterCursor = bs}

flipAroundRow :: EditState -> EditState
flipAroundRow editState =
  let beforeLines = stableLines . beforeCursor $ editState
      afterLines = stableLines . afterCursor $ editState
      flipped ls = map reverse $ tail ls
      flippedToAfter = flipped beforeLines
      flippedToBefore = flipped afterLines
      stillBefore = head beforeLines
      stillAfter = head afterLines
   in EditState
        { beforeCursor = stableUnlines $ [stillBefore] ++ flippedToBefore
        , afterCursor = stableUnlines $ [stillAfter] ++ flippedToAfter
        }

moveRight :: EditState -> EditState
moveRight EditState {beforeCursor = bs, afterCursor = a:as} =
  EditState {beforeCursor = a : bs, afterCursor = as}
moveRight s = s

moveLeft :: EditState -> EditState
moveLeft = flipAroundCursor . moveRight . flipAroundCursor

moveDown :: EditState -> EditState
moveDown editState@EditState {beforeCursor = bs, afterCursor = as} =
  let col = column editState
      offset =
        fromMaybe 0 $ do
          (startIdx, endIdx) <- nextLine editState
          return $
            if col <= endIdx - startIdx
              then startIdx + col
              else endIdx
      (skipped, left) = splitAt offset as
   in EditState {beforeCursor = reverse skipped ++ bs, afterCursor = left}

moveUp :: EditState -> EditState
moveUp = flipAroundRow . moveDown . flipAroundRow

pushEdit :: Char -> EditState -> EditState
pushEdit c EditState {beforeCursor = bs, afterCursor = as} =
  EditState {beforeCursor = c : bs, afterCursor = as}

pushEdits :: String -> EditState -> EditState
pushEdits str editState = foldl (flip pushEdit) editState str

popEdit :: EditState -> EditState
popEdit EditState {beforeCursor = b:bs, afterCursor = as} =
  EditState {beforeCursor = bs, afterCursor = as}
popEdit s = s

popEdits :: Int -> EditState -> EditState
popEdits 0 editState = editState
popEdits n editState = popEdits (n - 1) $ popEdit editState

-- IO Functions
start :: IO ()
start = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  run vty emptyEditState

run :: Vty -> EditState -> IO ()
run vty editState = do
  render vty editState
  (shouldExit, editState) <- handleEvent vty editState
  if shouldExit
    then shutdown vty
    else run vty editState

handleEvent :: Vty -> EditState -> IO (Bool, EditState)
handleEvent vty editState = do
  event <- nextEvent vty
  case event of
    EvKey KEsc [] -> return (True, editState)
    EvKey KLeft [] -> return (False, moveLeft editState)
    EvKey KRight [] -> return (False, moveRight editState)
    EvKey KDown [] -> return (False, moveDown editState)
    EvKey KUp [] -> return (False, moveUp editState)
    EvKey KEnter [] -> return (False, pushEdit '\n' editState)
    EvKey KBS [] -> return (False, popEdit editState)
    EvKey (KChar '\t') [] -> return (False, pushEdits "    " editState)
    EvKey (KChar c) [] -> return (False, pushEdit c editState)
    _ -> return (False, editState)

render :: Vty -> EditState -> IO ()
render vty editState = do
  let pic = picForImage $ renderEditState editState
  update vty pic

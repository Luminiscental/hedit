module Lib
  ( start
  ) where

import Data.List
import Graphics.Vty

start :: IO ()
start = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  run vty emptyEditState

data EditState = EditState
  { beforeCursor :: String
  , afterCursor :: String
  }

emptyEditState :: EditState
emptyEditState = EditState {beforeCursor = "", afterCursor = ""}

moveRight :: EditState -> EditState
moveRight EditState {beforeCursor = bs, afterCursor = a:as} =
  EditState {beforeCursor = a : bs, afterCursor = as}
moveRight s = s

moveLeft :: EditState -> EditState
moveLeft EditState {beforeCursor = b:bs, afterCursor = as} =
  EditState {beforeCursor = bs, afterCursor = b : as}
moveLeft s = s

pushEdit :: Char -> EditState -> EditState
pushEdit c EditState {beforeCursor = bs, afterCursor = as} =
  EditState {beforeCursor = c : bs, afterCursor = as}

popEdit :: EditState -> EditState
popEdit EditState {beforeCursor = b:bs, afterCursor = as} =
  EditState {beforeCursor = bs, afterCursor = as}
popEdit s = s

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
    EvKey (KChar c) [] -> return (False, pushEdit c editState)
    EvKey KEnter [] -> return (False, pushEdit '\n' editState)
    EvKey KBS [] -> return (False, popEdit editState)
    _ -> return (False, editState)

textAttr :: Attr
textAttr = defAttr `withForeColor` blue `withBackColor` black

cursorAttr :: Attr
cursorAttr = defAttr `withForeColor` green `withBackColor` black

cursorImg :: Image
cursorImg = string cursorAttr "|"

textToImg :: String -> Image
textToImg = string textAttr

stableLines :: String -> [String]
stableLines "" = [""]
stableLines str =
  let appended = str ++ "%"
      appendedLines = lines appended
   in init appendedLines ++ [init (last appendedLines)]

strToImgs :: String -> [Image]
strToImgs "" = [emptyImage]
strToImgs s = map textToImg . stableLines $ s

stackImgs :: [Image] -> Image
stackImgs = foldl (<->) emptyImage

renderEditState :: EditState -> Image
renderEditState editState =
  let imgsBefore = strToImgs $ reverse . beforeCursor $ editState
      imgsAfter = strToImgs . afterCursor $ editState
      middleImage = last imgsBefore <|> cursorImg <|> head imgsAfter
   in stackImgs
        [stackImgs (init imgsBefore), middleImage, stackImgs (tail imgsAfter)]

render :: Vty -> EditState -> IO ()
render vty editState = do
  let pic = picForImage $ renderEditState editState
  update vty pic

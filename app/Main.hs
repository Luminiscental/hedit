module Main where

import           Lib
import           Graphics.Vty

main :: IO ()
main = do
    cfg       <- standardIOConfig
    vty       <- mkVty cfg
    editState <- loadEditState
    runEditor vty editState
    shutdown vty

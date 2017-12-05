module Main where

import Lib

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
main :: IO ()
main = do
  putStr "Geben Sie bitte einen Dateinamen an: "
  fileName <- getLine
  openFile fileName
  putStrLn (" Die Datei " ++ fileName ++ " soll geoeffnet werden.")
--  hSetBuffering stdout NoBuffering

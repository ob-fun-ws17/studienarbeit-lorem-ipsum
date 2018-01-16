module Main where

import Lib


--import Data.Array.IO
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
main :: IO ()
main = do
  putStr "Geben Sie bitte einen Dateinamen an: "
  fileName <- getLine
  openFile fileName
  putStrLn (" Die Datei " ++ fileName ++ " soll geoeffnet werden.")
--  hSetBuffering stdout NoBuffering

mainA :: Occurences
mainA = do
  let alphabet = initAlphabet
  incrementOccurences 'A' 'A' alphabet

mainTest :: IO ()
mainTest = do
  let a = [('Z', 2), ('P', 3), ('A', 7), ('K', 11), ('N', 13), ('M', 17), ('I', 19), ('C', 23), ('H', 29)]
  print a
  print $ kummulate a

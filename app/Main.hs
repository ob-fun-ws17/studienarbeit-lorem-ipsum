{-|
Module      : Main
Description : Starts the Lorem-Ipsum-Generator
Maintainer  : Hübner Martin, Seeholzer Florian
-}
module Main where

import Lib

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
main :: IO ()
main = do
  putStrLn ""
  content <- openFile "fileName"
  putStrLn ""


mainTest :: IO ()
mainTest = do
  let a = [('A', ('Z', 2.0)),  ('A', ('A', 7.0)), ('A', ('K', 11.0)), ('A', ('N', 13.0)), ('A', ('M', 17.0)), ('A', ('I', 19.0)), ('A', ('C', 23.0)), ('A', ('H', 29.0))]
  let b = [('B', ('Z', 2.0)), ('B', ('P', 3.0)), ('B', ('A', 7.0)), ('B', ('K', 11.0)), ('B', ('N', 13.0)), ('B', ('M', 17.0)), ('B', ('I', 19.0)), ('B', ('C', 23.0)), ('B', ('H', 29.0))]
  let c = [('C', ('Z', 2.0)), ('C', ('P', 3.0)), ('C', ('A', 7.0)), ('C', ('K', 11.0)), ('C', ('N', 13.0)), ('C', ('M', 17.0)), ('C', ('I', 19.0)), ('C', ('C', 23.0)), ('C', ('H', 29.0))]
  let abc = [a,b,c]
  print a
  print b
  print c
  print "---"
  print abc
  print "---"

mainTestGetTuple :: IO ()
mainTestGetTuple = do

  let emptyLine = "\n" ++ "\n"

  let a = [('A', ('P',0.199)), ('A', ('K',0.399)), ('A', ('A',0.656)), ('A', ('C',0.682)), ('A', ('H',1.0))]
  let b = [('B', ('T',0.101)), ('B', ('R',0.211)), ('B', ('M',0.392)), ('B', ('I',0.787)), ('B', ('C',1.0))]
  let c = [('C', ('Z',0.115)), ('B', ('Z',0.315)), ('C', ('P',0.667)), ('C', ('K',0.677)), ('C', ('N',1.0))]

  putStr emptyLine

  print "----- Beginn der Zauberei -------------------"

  originalListe <- openFile ""
  print originalListe
  let originalListe = [a,b,c]
  putStr emptyLine
  print "OriginalListe - Die Liste im Original"
  print originalListe

  putStr emptyLine
  print "OriginalListe - ... nochmal formatiert"
  mapM_ print originalListe

  let charTupleListTuple = (fmap makeCharTupleListTuple originalListe)
  putStr emptyLine
  print "Liste umgebaut von [[(Char, (Char, Float))]] nach [(Char, [(Char, Float)])]"
  print charTupleListTuple

  putStr emptyLine
  print "Liste umgebaut von [[(Char, (Char, Float))]] nach [(Char, [(Char, Float)])] - nochmal formatiert"
  mapM_ print charTupleListTuple



  let ganzcharTupleListTuple = (filter (\tup -> fst tup == 'B') charTupleListTuple)!!0
  putStr emptyLine
  print "Das relevante Tupel der Liste, unter der Annahme, dass der letzte Eintrag / Buchstabe ein 'B' ist"
  print ganzcharTupleListTuple

  putStr emptyLine
  print "Der Zufallswert:"
  ranNum <- getRandomNumberOneTo
  let limit = ((fromIntegral ranNum) / 1000)
  print limit

  let verbliebeneListe = (filter (\tup -> snd tup > limit) (snd ganzcharTupleListTuple))
  putStr emptyLine
  let message = "Liste der Tupel, deren FloatWert groesser als " ++ show(limit) ++ " ist"
  print message
  print verbliebeneListe

  let konkretesTupel = head verbliebeneListe
  putStr emptyLine
  print "Das konkrete Tupel (sprich der letzte Eintrag)"
  print konkretesTupel

  let konkreterCharakter = fst konkretesTupel
  putStr emptyLine
  let message = "Der konkrete Charakter (hier: " ++ konkreterCharakter : [] ++ ")"
  print message
  print konkreterCharakter

  putStr emptyLine
  print "----- Ende der Zauberei -------------------"
  putStr emptyLine

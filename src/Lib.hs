module Lib where

    import System.IO
    import qualified Data.Map as M

    type Key    = Char
    type Occurences  = M.Map Key Integer
    type Letter = Char
    type Alphabet = M.Map Letter Occurences

    alphabet :: Alphabet
    alphabet = M.fromList[('A', M.fromList[('A', 0)])]

    initOccurences :: Occurences
    initOccurences = foldr(uncurry M.insert) M.empty $ zip ['A', 'B'..'Z'] $ repeat 0

    initAlphabet :: Alphabet
    initAlphabet = foldr(uncurry M.insert) M.empty $ zip ['A', 'B'..'Z'] $ repeat initOccurences

    openFile :: String -> IO ()
    openFile fileName = withFile "LoremIpsum.txt" ReadMode $ \handle -> do
                  xs <- getlines handle
                  sequence_ $ map putStrLn xs

    getlines :: Handle -> IO [String]
    getlines h = hGetContents h >>= return . lines

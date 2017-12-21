module Lib where

    import System.IO
    import qualified Data.Map as M

    type Key           = Char
    type Occurences    = M.Map Key Integer
    type Letter        = Char
    type Alphabet      = M.Map Letter Occurences

    occurences = initOccurences
    alphabet = initAlphabet

    initOccurences :: Occurences
    initOccurences = foldr(uncurry M.insert) M.empty $ zip ['A'..'Z'] $ repeat 0

    initAlphabet :: Alphabet
    initAlphabet = foldr(uncurry M.insert) M.empty $ zip ['A'..'Z'] $ repeat occurences

    openFile :: String -> IO ()
    openFile fileName = withFile "LoremIpsum.txt" ReadMode $ \handle -> do
                  xs <- getlines handle
                  sequence_ $ map putStrLn xs

    getlines :: Handle -> IO [String]
    getlines h = hGetContents h >>= return . lines

    getLength :: String -> Int
    getLength s = length s

    --getOccurence :: Key -> Integer
    --getOccurence x = (M.findWithDefault 0 x initOccurences)

    --incrementAlphabet :: Key -> Alphabet
    --incrementAlphabet = M.updateWithKey 'A' alphabet

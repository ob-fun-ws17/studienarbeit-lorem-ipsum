module Lib where

    import System.IO
    import qualified Data.Map.Strict as M

    type Key           = Char
    type Occurences    = M.Map Key Integer
    type Letter        = Char
    type Alphabet      = M.Map Letter Occurences

    occurences = initOccurences
    alphabet = initAlphabet

    initOccurences :: Occurences
    initOccurences = foldr(uncurry M.insert) M.empty $ zip ['A'..'Z'] $ repeat 0

    initAlphabet :: Alphabet
    initAlphabet = foldr(uncurry M.insert) M.empty $ zip ['A'..'Z'] $ repeat initOccurences

    openFile :: String -> IO ()
    openFile fileName = withFile "LoremIpsum.txt" ReadMode $ \handle -> do
                  xs <- getlines handle
                  sequence_ $ map putStrLn xs

    getlines :: Handle -> IO [String]
    getlines h = hGetContents h >>= return . lines

    getLength :: String -> Int
    getLength s = length s

    getOccurence :: Key -> Integer
    getOccurence x = (M.findWithDefault 0 x occurences)

    getParamtersOfKey :: Key -> Occurences
    getParamtersOfKey x = M.findWithDefault occurences x alphabet

    --getAlphabet :: Key -> [Integer]
    --getAlphabet x = map M.findWithDefault 0 x occurences

    -- incrementOccurences :: Char -> Occurences -> Occurences
    -- incrementOccurences x y = M.insert x ((getOccurence x) + 1) y

    incrementOccurences :: Char -> Occurences
    incrementOccurences x = M.insert x ((getOccurence x) + 1) occurences

module Lib where

    import System.IO
    import qualified Data.Map.Strict as M
    import Data.Char
    import Data.List
    import Data.Function

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

    frequency :: (Ord a) => [a] -> [(a, Int)]
    frequency xs = M.toList (M.fromListWith (+) [(x, 1) | x <- xs])

    lookup :: Char -> [(Char,Int)] -> Int
    lookup x zs = (head [b | (a,b) <- zs, (a==x)])

    getCharCountValue :: Char -> [(Char, Int)] -> Int
    getCharCountValue key  freqList = Lib.lookup key freqList

    swapTupleValues :: ((Char, Char), Int) -> (Char, (Char, Int))
    swapTupleValues ((a, b), c) = (a, (b, c))

    changeIntTupleToFloat :: [(Char, Int)] -> (Char,(Char, Int)) -> (Char,(Char, Float))
    changeIntTupleToFloat freqList (a, (b, c)) = (a,(b, (fromIntegral c) / (fromIntegral (getCharCountValue a freqList))))

    countLetters :: String -> Char -> Int
    countLetters str c = length $ filter (== c) str

    printFloatTuple :: (Char, (Char, Float)) -> String
    printFloatTuple (a, (b, c)) = charToString a ++ " " ++ charToString b ++ " " ++ show c

    groupTupleLists :: [(Char, (Char, Float))] -> [[(Char, (Char, Float))]]
    groupTupleLists list = groupBy (\a b -> fst a == fst b) list

    openFile :: String -> IO ()
    openFile fileName = do
      content <- readFile "LoremIpsum.txt"
      let content' = map toUpper $ filter isAlpha content
      let charCounts = frequency content'
      let zipped = zip content' $ tail content'
      let grouped = frequency zipped
      let swapped = map swapTupleValues grouped
      let changed = map (changeIntTupleToFloat charCounts) (swapped)
      let groupedTuples = groupTupleLists changed

      print groupedTuples

    readChars :: Int -> String -> Alphabet-> Occurences
    readChars index content alph = incrementOccurences (content!!index) (content!!(index+1)) alph

    charToString :: Char -> String
    charToString c = [c]

    printXs :: String -> IO()
    printXs xs = do putStrLn xs

    getOccurence :: Key -> Occurences -> Integer
    getOccurence key occ = M.findWithDefault 0 key occ

    getOccurences :: Key -> Alphabet -> Occurences
    getOccurences key alph = M.findWithDefault occurences key alph

    incrementOccurences :: Key -> Key -> Alphabet -> Occurences
    incrementOccurences key1 key2 alph = M.insert key1 ((getOccurence key1 (getOccurences key2 alph)) + 1) (getOccurences key2 alph)

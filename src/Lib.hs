{-|
Module      : Lib
Description : Contains all functions. Generates a Lorem-Ipsum text based on random numbers.
Maintainer  : Hübner Martin, Seeholzer Florian
-}
module Lib where

    import System.IO
    import qualified Data.Map.Strict as M
    import Data.Char
    import Data.List
    import Data.Function

    import System.Random

    type Key           = Char
    type Occurences    = M.Map Key Integer
    type Letter        = Char
    type Alphabet      = M.Map Letter Occurences

    occurences = initOccurences

    -- | Initializes a new (inner) list of Char, Int tuples.
    initOccurences :: Occurences
    initOccurences = foldr(uncurry M.insert) M.empty $ zip ['A'..'Z'] $ repeat 0

    -- | Initializes a new (outer) list of Char, Occurences tuples.
    initAlphabet :: Alphabet
    initAlphabet = foldr(uncurry M.insert) M.empty $ zip ['A'..'Z'] $ repeat initOccurences

    -- | Used to get the number of all occurences of an datatype.
    frequency :: (Ord a) => [a] -> [(a, Int)]
    frequency xs = M.toList (M.fromListWith (+) [(x, 1) | x <- xs])

    -- | Used by getCharCountValue to get the number of occurences from a specific character.
    lookup :: Char -> [(Char,Int)] -> Int
    lookup x zs = (head [b | (a,b) <- zs, (a==x)])

    -- | Returns the number of occurences from a specific character.
    getCharCountValue :: Char -> [(Char, Int)] -> Int
    getCharCountValue key  freqList = Lib.lookup key freqList

    -- | Swaps the three values of a tuple of tuples.
    swapTupleValues :: ((Char, Char), Int) -> (Char, (Char, Int))
    swapTupleValues ((a, b), c) = (a, (b, c))


    -- | Changes the old list with int occurences to a new list with float percentages (0.0 - 1.0).
    changeIntTupleToFloat :: [(Char, Int)] -> (Char,(Char, Int)) -> (Char,(Char, Float))
    changeIntTupleToFloat freqList (a, (b, c)) = (a,(b, (fromIntegral c) / (fromIntegral (getCharCountValue a freqList))))

    -- | Counts the occurences of a char value in a string.
    countLetters :: String -> Char -> Int
    countLetters str c = length $ filter (== c) str

    -- | Prints a tuple after the occurence count has been changed to the percentages (0.0 - 1.0).
    printFloatTuple :: (Char, (Char, Float)) -> String
    printFloatTuple (a, (b, c)) = charToString a ++ " " ++ charToString b ++ " " ++ show c

    -- | Changes the list's structure.
    groupTupleLists :: [(Char, (Char, Float))] -> [[(Char, (Char, Float))]]
    groupTupleLists list = groupBy (\a b -> fst a == fst b) list

    -- | Each character has it's own 'area' between 0.0 and 1.0. This is used to ensure better result from our randomizer.
    kummulate :: [(Char, Float)] -> [(Char, Float)]
    kummulate xs = do
      let headXs = head xs
      let tailXs = tail xs
      if length xs <= 1
      then xs
      else headXs : kummulate((kummulateAddNumber (head tailXs) (snd headXs)) : tail tailXs)

    -- | Returns the sum of both tuple values and another value.
    kummulateAddNumber :: (Char, Float) -> Float -> (Char, Float)
    kummulateAddNumber tup num = (fst tup, snd tup + num)

    {-
    separate :: [[(Char, (Char, Float))]] -> Char
    separate xs = do
      if length xs <= 1 then
        getKeyOfTuple $ head xs
      else
        separate $ tail xs
    -}

    -- | Gets the key of a tuple.
    getKeyOfTuple :: [(Char, (Char, Float))] -> Char
    getKeyOfTuple xs = fst (xs!!0)

    -- | Gets the inner tuple of a tuple.
    getSubTupelOfTuple :: (Char, (Char, Float)) -> (Char, Float)
    getSubTupelOfTuple tup = snd (tup)

    -- | Changes the list to a list of tuples.
    makeListOfTuple :: [(Char, (Char, Float))] -> [(Char, Float)]
    makeListOfTuple xs = fmap getSubTupelOfTuple xs

    -- | Changes the list of tuples a tuples with a list as the second argument.
    makeCharTupleListTuple :: [(Char, (Char, Float))] -> (Char, [(Char, Float)])
    makeCharTupleListTuple a = (getKeyOfTuple a, kummulate(makeListOfTuple a))

    -- | Used to start the whole process.
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
      let optimizedList = optimizeList groupedTuples
      generateText "" 1000 'E' optimizedList

    -- | Changes a char to a string.
    charToString :: Char -> String
    charToString c = [c]

    -- | Returns a random Int (1-1000).
    getRandomNumberOneTo = randomRIO (1,1000) :: IO Int

    -- | Changes the list's structure for further use.
    optimizeList :: [[(Char, (Char, Float))]] -> [(Char, [(Char, Float)])]
    optimizeList list = do
      let originalListe = list
      fmap makeCharTupleListTuple originalListe

    -- | Generates a text based on random numbers and the occurence values of the characters.
    generateText :: String -> Int -> Char -> [(Char, [(Char, Float)])] -> IO ()
    generateText result count start dat = do
      let ganzcharTupleListTuple = (filter (\tup -> fst tup == start) dat)!!0
      ranNum <- getRandomNumberOneTo
      let limit = ((fromIntegral ranNum) / 1000)
      let tmpList1 = (filter (\tup -> snd tup >= limit) (snd ganzcharTupleListTuple))

      let tmpList2 = (filter (\tup -> snd tup >= 0) (snd ganzcharTupleListTuple))

      let verbliebeneListe | (length tmpList1) == 0 = tmpList2 | otherwise = tmpList1

      let konkretesTupel = head verbliebeneListe
      let konkreterCharakter = fst konkretesTupel

      if (count > 0)
        then do
          (generateText (result ++ [konkreterCharakter]) (count-1) konkreterCharakter dat)
        else print result

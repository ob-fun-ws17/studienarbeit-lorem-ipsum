{-|
Module      : Lorem Ipsum Module
Description : Contains all functions
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

    kummulate :: [(Char, Float)] -> [(Char, Float)]
    kummulate xs = do
      let headXs = head xs
      let tailXs = tail xs
      if length xs <= 1
      then xs
      else headXs : kummulate((kummulateAddNumber (head tailXs) (snd headXs)) : tail tailXs)

    kummulateAddNumber :: (Char, Float) -> Float -> (Char, Float)
    kummulateAddNumber tup num = (fst tup, snd tup + num)

    separate :: [[(Char, (Char, Float))]] -> Char
    separate xs = do
      if length xs <= 1 then
        getKeyOfTuple $ head xs
      else
        separate $ tail xs

    getKeyOfTuple :: [(Char, (Char, Float))] -> Char
    getKeyOfTuple xs = fst (xs!!0)

    getSubTupelOfTuple :: (Char, (Char, Float)) -> (Char, Float)
    getSubTupelOfTuple tup = snd (tup)

    makeListOfTuple :: [(Char, (Char, Float))] -> [(Char, Float)]
    makeListOfTuple xs = fmap getSubTupelOfTuple xs

    --makeTupleOfCharAndListOfTuple :: [(Char, (Char, Float))] -> (Char, [(Char, Float)])
    --makeTupleOfCharAndListOfTuple xs = (getKeyOfTuple xs, makeListOfTuple xs)

    makeCharTupleListTuple :: [(Char, (Char, Float))] -> (Char, [(Char, Float)])
    makeCharTupleListTuple a = (getKeyOfTuple a, kummulate(makeListOfTuple a))

--    kummulatesLittleHelper :: [[(Char, (Char, Float))]] -> [(Char, Float)]
--    kummulatesLittleHelper [[(key1, (key2, flo))]] = [(key2, flo)]

    --openFile :: String -> [[(Char, (Char, Float))]]
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

      return ()

      --let allAdded = map kummulate groupedTuples
      --print allAdded
      --print groupedTuples
      --return (Just groupedTuples)


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

    --tenPseudorandomNumbers :: Int -> [Int]
    --tenPseudorandomNumbers = take 10 . randomRs (0, 99) . mkStdGen $ newStdGen

--    getRandomNumberOneTo :: Int
    getRandomNumberOneTo = randomRIO (1,1000) :: IO Int

--    getRandomNumber :: Float
--    getRandomNumber = do
--      num <- randomIO :: IO Float
--      return (myPureFunction num)

    optimizeList :: [[(Char, (Char, Float))]] -> [(Char, [(Char, Float)])]
    optimizeList list = do
      let originalListe = list
      fmap makeCharTupleListTuple originalListe

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


      --if count <= 0
        --then ([konkreterCharakter] ++ (generateText (count-1) konkreterCharakter dat))
        --else [konkreterCharakter] ++ []


    {-|
    getRandNum :: IO Int
    getRandNum = do
          ranNum <- getRandomNumberOneTo
          let limit = ((fromIntegral ranNum) / 1000)
          return limit
    -}

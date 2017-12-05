module Lib where

    import System.IO

    openFile :: String -> IO ()
    openFile fileName = withFile "LoremIpsum.txt" ReadMode $ \\handle -> do
                  xs <- getlines handle
                  sequence_ $ map putStrLn xs

    getlines :: Handle -> IO [String]
    getlines h = hGetContents h >>= return . lines

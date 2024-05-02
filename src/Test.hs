module Test where

tScan :: [Int] -> [Int -> Int]
tScan = scanl tComb id

tComb :: (Int -> Int) -> Int -> (Int -> Int)
tComb f jprev j = f j - tProj (f jprev) j

tProj :: Int -> Int -> Int
tProj = (*)
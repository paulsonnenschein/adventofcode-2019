lower, upper :: Int
(lower, upper) = (377777, 799999) -- (372304, 847060)

type Digits = (Int, Int, Int, Int, Int, Int)

main :: IO ()
main = do 
    let output = show (lower, upper)
    putStrLn "Part 1:"
    putStrLn . show . length $ filteredPW
    putStrLn "Part 2:"
    putStrLn . show . length $ filteredPWP2

genPasswords :: [Digits]
genPasswords = map convertToDigits [lower..upper]

filteredPW :: [Digits]
filteredPW = filter filterPW genPasswords

filteredPWP2 :: [Digits]
filteredPWP2 = filter filterPWP2 filteredPW

filterPW :: Digits -> Bool
filterPW pw = isAscending && hasNeighboring
    where (d1,d2,d3,d4,d5,d6) = pw
          isAscending = d1 <= d2 && d2 <= d3 && d3 <= d4 && d4 <= d5 && d5 <= d6
          hasNeighboring = d1 == d2 || d2 == d3 || d3 == d4 || d4 == d5 || d5 == d6

filterPWP2 :: Digits -> Bool
filterPWP2 pw = (d1 == d2 && d2 /= d3) || (d1 /= d2 && d2 == d3 && d3 /= d4) || (d2 /= d3 && d3 == d4 && d4 /= d5) || (d3 /= d4 && d4 == d5 && d5 /= d6) || (d4 /= d5 && d5 == d6)
    where (d1,d2,d3,d4,d5,d6) = pw
          

convertToDigits :: Int -> Digits
convertToDigits pw = (position 5,position 4,position 3,position 2,position 1,position 0)
    where position :: Int -> Int
          position n = pw `div` (10^n) `mod` 10
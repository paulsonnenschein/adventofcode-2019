main :: IO ()
main = interact runParts

runParts :: String -> String
runParts input =
  let parsed = map read . split ',' $ input :: [Int]
  in  unlines
        ["Part 1:", show . part1 $ parsed, "Part 2: ", show . part2 $ parsed]

part1 :: [Int] -> Int
part1 = runParameterized 12 2 -- preparation list[1] = 12, list[2] = 2

part2 :: [Int] -> Int
part2 input = inner possibilities
 where
  possibilities = [ (x, y) | x <- [0 .. 99], y <- [0 .. 99] ]
  inner ((a, b) : ls) | runParameterized a b input == 19690720 = 100 * a + b
                      | otherwise                              = inner ls
  inner [] = error "none matched"

split :: Char -> String -> [String]
split _ []  = []
split c str = words . replaceSapces $ str
 where
  replaceSapces [] = []
  replaceSapces (h : ls) | h == c    = ' ' : replaceSapces ls
                         | otherwise = h : replaceSapces ls

runParameterized :: Int -> Int -> [Int] -> Int
runParameterized noun verb = head . eval . setAt 2 verb . setAt 1 noun

eval :: [Int] -> [Int]
eval input = eval' input 0

eval' :: [Int] -> Int -> [Int]
eval' list offset = case drop offset list of
  (1 : a : b : c : _) -> eval' (setAt c (addAt a b list) list) (offset + 4)
  (2 : a : b : c : _) -> eval' (setAt c (mulAt a b list) list) (offset + 4)
  (99            : _) -> list
  _                   -> error "invalid patern"

setAt :: Int -> a -> [a] -> [a]
setAt n newEl list = let (ys, _ : zs) = splitAt n list in ys ++ [newEl] ++ zs

addAt :: Num a => Int -> Int -> [a] -> a
addAt a b list = (list !! a) + (list !! b)

mulAt :: Num a => Int -> Int -> [a] -> a
mulAt a b list = (list !! a) * (list !! b)


main :: IO ()
main = interact runParts

runParts :: String -> String
runParts input =
  let parsed = map read . split ',' $ input :: [Int]
  in  unlines ["Part 1:", show . part1 $ parsed]

part1 :: [Int] -> Int
part1 list =
  let prepared = setAt 2 2 . setAt 1 12 $ list -- preparation list[1] = 12, list[2] = 2
  in  head . eval $ prepared

split :: Char -> String -> [String]
split _ []  = []
split c str = words . replaceSapces $ str
 where
  replaceSapces [] = []
  replaceSapces (h : ls) | h == c    = ' ' : replaceSapces ls
                         | otherwise = h : replaceSapces ls

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


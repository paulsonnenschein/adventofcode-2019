import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)

main :: IO ()
main = interact runParts

type Coord = (Int, Int) -- (x,y)
data Instr = R Int | U Int | L Int | D Int deriving Show

runParts :: String -> String
runParts input = unlines ["Part 1:", show part1, "Part 2:", show part2]
 where
  [path1, path2] = map (tail . evaluatePath . parseLine) . lines $ input
  matching       = getMatching path1 path2
  part1          = manhattan . foldr1 minDistance $ matching
  distances      = map (\c -> (getPathLengthTo c path1, getPathLengthTo c path2)) matching
  part2          = manhattan . foldr1 minDistance $ distances

parseLine :: String -> [Instr]
parseLine = map parseInstr . words . replaceCommas
    where replaceCommas = map (\c -> case c of ',' -> ' '; _ -> c)

parseInstr :: String -> Instr
parseInstr ('R' : val) = R (read val)
parseInstr ('U' : val) = U (read val)
parseInstr ('L' : val) = L (read val)
parseInstr ('D' : val) = D (read val)
parseInstr _           = error "Invalid instruction"

getMatching :: [Coord] -> [Coord] -> [Coord]
getMatching path1 path2 = inner (sort path1) (sort path2)
 where
  -- advance until both are equal, lists need to be sorted
  inner :: [Coord] -> [Coord] -> [Coord]
  inner [] _  = []
  inner _  [] = []
  inner (h1 : ls1) (h2 : ls2) | h1 < h2   = inner ls1 (h2 : ls2)
                              | h1 > h2   = inner (h1 : ls1) ls2
                              | otherwise = h1 : inner ls1 ls2


minDistance :: Coord -> Coord -> Coord
minDistance a b = if manhattan a < manhattan b then a else b

getPathLengthTo :: Coord -> [Coord] -> Int
getPathLengthTo target path = (1+) . fromJust $ elemIndex target path

evaluatePath :: [Instr] -> [Coord]
evaluatePath line = inner line [(0, 0)] (0, 0)
    where inner :: [Instr] -> [Coord] -> Coord -> [Coord]
          inner [] path _ = path
          inner (R val:ls) path (x,y) = inner ls (path ++ genRights val (x,y)) (x+val,y)
          inner (U val:ls) path (x,y) = inner ls (path ++ genUps val (x,y)) (x,y+val)
          inner (L val:ls) path (x,y) = inner ls (path ++ genLefts val (x,y)) (x-val,y)
          inner (D val:ls) path (x,y) = inner ls (path ++ genDowns val (x,y)) (x,y-val)

genMod :: Coord -> Int -> Coord -> [Coord]
genMod (dx, dy) 1 (x, y) = [(x + dx, y + dy)]
genMod (dx, dy) n (x, y) = (x + dx, y + dy) : genMod (dx, dy) (n - 1) (x + dx, y + dy)

genUps, genDowns, genRights, genLefts :: Int -> Coord -> [Coord]
genUps = genMod (0, 1)
genDowns = genMod (0, -1)
genRights = genMod (1, 0)
genLefts = genMod (-1, 0)

manhattan :: Coord -> Int
manhattan (x, y) = abs x + abs y

import Debug.Trace (trace)
import Data.List (sort)

main :: IO ()
main = interact runParts

type Coord = (Int, Int) -- (x,y)
data Instr = R Int | U Int | L Int | D Int deriving Show

runParts :: String -> String
runParts input = unlines ["Part 1:", show.part1 $ (line1,line2)]
    where [line1, line2] = map parseLine . lines $ input

parseLine :: String -> [Instr]
parseLine = map parseInstr . words . replaceCommas
    where replaceCommas = map (\c -> case c of ',' -> ' '; _ -> c)

parseInstr :: String -> Instr
parseInstr ('R' : val) = R (read val)
parseInstr ('U' : val) = U (read val)
parseInstr ('L' : val) = L (read val)
parseInstr ('D' : val) = D (read val)
parseInstr _           = error "Invalid instruction"

part1 :: ([Instr], [Instr]) -> Int
part1 (line1, line2) = manhattan min
 where
  path1    = sort . tail . evaluatePath $ line1
  path2    = sort . tail . evaluatePath $ line2
  matching = getMatching path1 path2
  min = foldr1 (\a min -> if manhattan a < manhattan min then a else min) matching

getMatching :: [Coord] -> [Coord] -> [Coord]
getMatching [] _  = []
getMatching _  [] = []
getMatching (h1 : ls1) (h2 : ls2) | h1 < h2   = getMatching ls1 (h2 : ls2)
                                  | h1 > h2   = getMatching (h1 : ls1) ls2
                                  | otherwise = h1 : getMatching ls1 ls2


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

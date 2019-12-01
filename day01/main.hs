main :: IO ()
main = interact runParts

runParts :: String -> String
runParts s =
  let input = map read . lines $ s
  in  unlines ["Part1: " ++ show (part1 input), "Part2: " ++ show (part2 input)]


part1 :: [Int] -> Int
part1 = sum . map calculateFuel

calculateFuel :: Int -> Int
calculateFuel mass = mass `div` 3 - 2

part2 :: [Int] -> Int
part2 = sum . map calculateFuelFuel

calculateFuelFuel :: Int -> Int
calculateFuelFuel x
  | let fuel = calculateFuel x, fuel > 0 = fuel + calculateFuelFuel fuel
  | otherwise                            = 0

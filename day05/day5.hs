import Data.List

threeVowels = (>= 3) . length . flip intersect "aeiou"
doubleLetter s = or $ zipWith (==) s (drop 1 s)
noNaughtyPairs s = not $ any (flip isInfixOf s) ["ab", "cd", "pq", "xy"]

nice1 s = threeVowels s && doubleLetter s && noNaughtyPairs s

twoPair (a:s'@(b:s)) = [a,b] `isInfixOf` s || twoPair s'
twoPair _ = False

abaTest s = or $ zipWith (==) s (drop 2 s)

nice2 s = twoPair s && abaTest s

main = do
    input <- fmap lines $ readFile "input.txt"
    let count1 = length $ filter nice1 input
    putStrLn $ "nice rule 1: " ++ show count1
    let count2 = length $ filter nice2 input
    putStrLn $ "nice rule 2: " ++ show count2

{-
255
55
-}

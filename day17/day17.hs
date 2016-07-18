parse :: String -> [Int]
parse = map read . lines

subsetSum :: Int -> [Int] -> [[Int]]
subsetSum 0 _ = [[]]
subsetSum _ [] = []
subsetSum n (c:cs) = subsetSum n cs ++ if n >= c then map (c:) (subsetSum (n-c) cs) else []

main :: IO ()
main = do
    input <- parse <$> readFile "input.txt"
    let combos = subsetSum 150 input
        comboLens = map length combos
        minCombos = filter (== minimum comboLens) comboLens
    putStrLn $ "total combinations: " ++ show (length combos)
    putStrLn $ "combinations with min containers: " ++ show (length minCombos)

{-
total combinations: 1304
combinations with min containers: 18
-}

parse :: String -> [Int]
parse = map read . lines

naive 0 _ = [[]]
naive _ [] = []
naive n (c:cs) = naive n cs ++ if n >= c then map (c:) (naive (n-c) cs) else []

main = do
    input <- parse `fmap` readFile "input.txt"
    print input
    let combos = naive 150 input
        minLen = minimum $ map length combos
        minCombos = filter ((== minLen) . length) combos
    putStrLn $ "total combinations: " ++ show (length combos)
    putStrLn $ "minimum containers: " ++ show minLen
    putStrLn $ "combinations with min containers: "
        ++ show (length minCombos)
    print minCombos
    

{-
1304
18
-}

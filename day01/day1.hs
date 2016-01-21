floorChange '(' = [1]
floorChange ')' = [-1]
floorChange _   = []

floorTrace = scanl (+) 0 . concatMap floorChange

main = do
    ft <- floorTrace `fmap` readFile "input.txt"
    putStrLn $ "final floor: " ++ show (last ft)
    putStrLn $ "first step into basement @ " ++ show (length $ takeWhile (>= 0) ft)

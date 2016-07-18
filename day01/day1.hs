import Data.Maybe (mapMaybe)

floorTrace :: String -> [Int]
floorTrace = scanl (+) 0 . mapMaybe floorChange
    where
        floorChange '(' = Just 1
        floorChange ')' = Just (-1)
        floorChange _   = Nothing

main :: IO ()
main = do
    ft <- floorTrace <$> readFile "input.txt"
    putStrLn $ "final floor: " ++ show (last ft)
    putStrLn $ "first step into basement @ " ++ show (length $ takeWhile (>= 0) ft)

{-
final floor: 74
first step into basement @ 1795
-}

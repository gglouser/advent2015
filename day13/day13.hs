import Data.List (permutations)
import qualified Data.Map.Strict as Map

type Relations = Map.Map String (Map.Map String Int)

parse :: String -> Relations
parse = Map.fromListWith (Map.unionWith (+)) . concatMap (entry . line . words) . lines
    where
        line [a,_,"gain",n,_,_,_,_,_,_,b] = (a, init b, read n)
        line [a,_,"lose",n,_,_,_,_,_,_,b] = (a, init b, -(read n))
        entry (a,b,n) = [(a, Map.singleton b n), (b, Map.singleton a n)]

happy :: Relations -> String -> String -> Int
happy m a b = (m Map.! a) Map.! b

totalHaps :: Relations -> [String] -> Int
totalHaps m = sum . (zipWith (happy m) <*> drop 1)

optimalHaps :: Relations -> ([String] -> [[String]]) -> Int
optimalHaps hapMap seatGen = maximum . map (totalHaps hapMap) . seatGen $ Map.keys hapMap

seatGen1 :: [a] -> [[a]]
seatGen1 (a:guests) = [a : gs ++ [a] | gs <- permutations guests]

seatGen2 :: [a] -> [[a]]
seatGen2 = permutations

main :: IO ()
main = do
    input <- parse <$> readFile "input.txt"
    putStrLn $ "optimal happiness 1: " ++ show (optimalHaps input seatGen1)
    putStrLn $ "optimal happiness 2: " ++ show (optimalHaps input seatGen2)

{-
optimal happiness 1: 733
optimal happiness 2: 725
-}

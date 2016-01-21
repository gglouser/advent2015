import Control.Applicative
import Data.List (permutations)
import qualified Data.Map.Strict as Map

parse :: String -> Map.Map String (Map.Map String Int)
parse = Map.fromListWith (Map.unionWith (+)) . concatMap (line . words) . lines
    where
        line [a,_,"gain",n,_,_,_,_,_,_,b] = entry a (init b) (read n)
        line [a,_,"lose",n,_,_,_,_,_,_,b] = entry a (init b) (-(read n))
        entry a b n = [(a, Map.singleton b n), (b, Map.singleton a n)]

happy m a b = (m Map.! a) Map.! b
totalHaps m guests = sum $ zipWith (happy m) guests (drop 1 guests)

optimalHaps hapMap seatGen = maximum . map (totalHaps hapMap) . seatGen $ Map.keys hapMap

seatGen1 (a:guests) = [a : gs ++ [a] | gs <- permutations guests]
seatGen2 guests = permutations guests

main = do
    input <- parse <$> readFile "input.txt"
    putStrLn $ "optimal happiness 1: " ++ show (optimalHaps input seatGen1)
    putStrLn $ "optimal happiness 2: " ++ show (optimalHaps input seatGen2)
    

{-
optimal happiness 1: 733
optimal happiness 2: 725
-}

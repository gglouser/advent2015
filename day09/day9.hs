import Control.Applicative
import Data.List (permutations)
import qualified Data.Map.Strict as Map

parse :: String -> Map.Map String (Map.Map String Int)
parse = Map.fromListWith Map.union . concatMap (entry . line . words) . lines
    where
        line l = (l !! 0, l !! 2, read (l !! 4))
        entry (a,b,d) = [(a, Map.singleton b d), (b, Map.singleton a d)]

dist m a b = (m Map.! a) Map.! b
pathLen m cities = sum $ zipWith (dist m) cities (drop 1 cities)

main = do
    input <- parse <$> readFile "input.txt"
    let cities = Map.keys input
        allPaths = permutations cities
        pathLens = map (pathLen input) allPaths
    putStrLn $ "minimum path: " ++ show (minimum pathLens)
    putStrLn $ "maximum path: " ++ show (maximum pathLens)

{-
minimum path: 207
maximum path: 804
-}

import Data.List (permutations)
import qualified Data.Map.Strict as Map

type Roads = Map.Map String (Map.Map String Int)

parse :: String -> Roads
parse = Map.fromListWith Map.union . concatMap (roads . line . words) . lines
    where
        line [a,_,b,_,d] = (a, b, read d)
        roads (a,b,d) = [(a, Map.singleton b d), (b, Map.singleton a d)]

dist :: Roads -> String -> String -> Int
dist m a b = (m Map.! a) Map.! b

pathLen :: Roads -> [String] -> Int
pathLen m = sum . (zipWith (dist m) <*> drop 1)

main :: IO ()
main = do
    input <- parse <$> readFile "input.txt"
    let pathLens = map (pathLen input) . permutations $ Map.keys input
    putStrLn $ "minimum path: " ++ show (minimum pathLens)
    putStrLn $ "maximum path: " ++ show (maximum pathLens)

{-
minimum path: 207
maximum path: 804
-}

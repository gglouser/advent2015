module Main where

import Lens.Micro
import System.Environment (getArgs)
import Text.Printf
import BBSearch
import SimSearch

-- Rough estimate of effective branching factor.
branchFactor :: Int -> Int -> Double
branchFactor n d = fromIntegral n ** (1 / fromIntegral d)

report :: BBResult SimState -> IO ()
report (BBResult Nothing n) = putStrLn $ printf "     no solution; %7d nodes expanded" n
report (BBResult (Just (m,s)) n) = putStrLn $ printf "%5d mana spent; %7d nodes expanded; EBF %f"
    m n (branchFactor n (s^.spellTrace.to length))

runSearch :: SimState -> (String, BBSearch SimState) -> IO ()
runSearch s (lbl, searcher) = do
    let part1 = searcher simObjective score roundPart1 s
        part2 = searcher simObjective score roundPart2 s
    putStr $ lbl ++ " - part 1: "
    report part1
    putStr $ lbl ++ " - part 2: "
    report part2

main :: IO ()
main = do
    args <- getArgs
    let (bhp, bdmg) = case args of (x:y:_) -> (read x, read y); _ -> (51, 9)
        initSimState = mkSimState bhp bdmg
    mapM_ (runSearch initSimState)
        [ ("DFS ", searchDFS)
        , ("DFS2", searchDFS2)
        , ("PQ  ", searchPQ)
        , ("FB  ", searchFBFS)
        ]

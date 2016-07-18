module Main where

import System.Environment (getArgs)
import BBSearch
import SimSearch

runSearch :: (SimState -> [SimState]) -> SimState -> IO ()
runSearch branch initState = do
    case searchDFS2 simObjective score branch initState of
        (BBResult Nothing _) -> putStrLn "no solution"
        (BBResult (Just (_,s)) _) -> do
            print $ _manaSpent s
            print $ _spellTrace s

main :: IO ()
main = do
    args <- getArgs
    let (bhp, bdmg) = case args of (x:y:_) -> (read x, read y); _ -> (51, 9)
        initSimState = mkSimState bhp bdmg
    putStr "part1: "
    runSearch roundPart1 initSimState
    putStr "part2: "
    runSearch roundPart2 initSimState

{-
900
1216
-}

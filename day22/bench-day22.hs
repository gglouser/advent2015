module Main where

import Criterion.Main
import BBSearch
import SimSearch

initState_51_9 = mkSimState 51 9
initState_55_8 = mkSimState 55 8
initState_58_9 = mkSimState 58 9
initState_71_10 = mkSimState 71 10

benchSearcher :: String -> BBSearch SimState -> Benchmark
benchSearcher label searcher = bgroup label $ do
    (state, stateLbl) <- [
        (initState_51_9, "51-9"),
        (initState_55_8, "55-8"),
        (initState_58_9, "58-9"),
        (initState_71_10, "71-10")]
    (part, partLbl) <- [(roundPart1, "part1"), (roundPart2, "part2")]
    let search = searcher simObjective score part
    return $ bench (stateLbl ++ "-" ++ partLbl) $ nf (fst3 . search) state

fst3 (x,_,_) = x

main :: IO ()
main = defaultMain [
    benchSearcher "DFS" searchDFS,
    benchSearcher "DFS2" searchDFS2 ]
    -- benchSearcher "PQ"  searchSimPQ,
    -- benchSearcher "FBFS" searchSimFBFS]

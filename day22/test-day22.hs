module Main where

import Control.Monad (when)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Printf
import BBSearch
import SimSearch

aocInputs :: [(Int, Int, [Int])]
aocInputs = [
    (51,  9, [ 900, 1216]),
    (55,  8, [ 953, 1289]),
    (58,  9, [1269, 1309]),
    (71, 10, [1824, 1937])]

testAOCInputs :: Test
testAOCInputs = TestList $ do
    searcher <- [searchDFS, searchDFS2, searchPQ, searchFBFS]
    (bHP, bDmg, answers) <- aocInputs
    let state = mkSimState bHP bDmg
    (n, part, ans) <- zip3 [1::Int ..] [roundPart1, roundPart2] answers
    let lbl = printf "boss hp %d, dmg %d, part %d" bHP bDmg n
    return $ lbl ~: ans ~=? (\(x,_,_) -> x) (searcher simObjective score part state)

main :: IO ()
main = do
    cs <- runTestTT testAOCInputs
    when (failures cs > 0) exitFailure

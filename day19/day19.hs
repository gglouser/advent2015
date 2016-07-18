import Data.List (sortBy, stripPrefix)
import Data.Ord (comparing)
import Data.Tuple (swap)
import qualified Data.Set as Set

parse :: String -> ([(String,String)], String)
parse = get . lines
    where
        get ls = (map (pline . words) (init $ init ls), last ls)
        pline l = (l!!0, l!!2)

replacePrefix :: String -> String -> String -> [String]
replacePrefix a b s = case stripPrefix a s of
                        Just s' -> [b ++ s']
                        Nothing -> []

replace :: String -> String -> String -> [String]
replace a b [] = []
replace a b s@(h:t) = replacePrefix a b s ++ map (h:) (replace a b t)

step :: [(String, String)] -> String -> [String]
step tfs s = concatMap (\(a,b) -> replace a b s) tfs

-----

reverseTransform :: [(String, String)] -> [(String, String)]
reverseTransform = reverse . sortBy (comparing (length . fst)) . map swap

revSearch :: [(String, String)] -> String -> [Int]
revSearch tfs "e" = return 0
revSearch tfs s = do
    (a,b) <- tfs
    s' <- replace a b s
    n <- revSearch tfs s'
    return (n+1)

-----

main :: IO ()
main = do
    input <- parse `fmap` readFile "input.txt"
    let (trans, medmol) = input
    -- Part 1
    print $ Set.size . Set.fromList $ step trans medmol
    -- Part 2
    -- Greedy solution works, but it is not *guaranteed* to work. :/
    let rtrans = reverseTransform trans
    print $ head $ revSearch rtrans medmol

example :: [(String, String)]
example = [("H", "HO"), ("H", "OH"), ("O", "HH"), ("e", "H"), ("e", "O")]

{-
535
212
-}

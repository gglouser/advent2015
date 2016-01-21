import Control.Applicative
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as P

type Stats a = Map.Map String a

parse :: P.Parsec String () [(Int, Stats Int)]
parse = paunt `P.endBy` P.endOfLine
    where
        pnum = read <$> some P.digit
        pitem = (,) <$> (some P.lower <* P.string ": ") <*> pnum
        paunt = (,) <$> (P.string "Sue " *> pnum <* P.string ": ")
                     <*> (Map.fromList <$> pitem `P.sepBy` P.string ", ")

mkChecker :: [String] -> [String] -> [(String, Int)] -> Stats (Int -> Bool)
mkChecker lbs ubs = Map.mapWithKey f . Map.fromList
    where
        f a | a `elem` lbs = (<)
            | a `elem` ubs = (>)
            | otherwise    = (==)

check :: Stats (Int -> Bool) -> Stats Int -> Bool
check m = Map.foldr' (&&) True . Map.intersectionWith ($) m

trueSue = [
    ("children", 3),
    ("cats", 7),
    ("samoyeds", 2),
    ("pomeranians", 3),
    ("akitas", 0),
    ("vizslas", 0),
    ("goldfish", 5),
    ("trees", 3),
    ("cars", 2),
    ("perfumes", 1)]

checker1 = mkChecker [] [] trueSue
checker2 = mkChecker ["cats", "trees"] ["pomeranians", "goldfish"] trueSue

main = do
    input <- readFile "input.txt"
    case P.parse parse "input.txt" input of
        Left err -> print err
        Right aunts -> do
            print $ filter (check checker1 . snd) aunts
            print $ filter (check checker2 . snd) aunts

{-
40
241
-}

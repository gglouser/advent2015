import Control.Applicative (some)
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as P

type Stats a = Map.Map String a

parse :: String -> Either P.ParseError [(Int, Stats Int)]
parse = P.parse aunts ""
    where
        aunts = aunt `P.endBy` P.endOfLine
        aunt = (,) <$> (P.string "Sue " *> num) <* P.string ": "
                    <*> (Map.fromList <$> item `P.sepBy` P.string ", ")
        item = (,) <$> some P.lower <* P.string ": " <*> num
        num = read <$> some P.digit

mkChecker :: [String] -> [String] -> [(String, Int)] -> Stats (Int -> Bool)
mkChecker lbs ubs = Map.mapWithKey f . Map.fromList
    where
        f a | a `elem` lbs = (<)
            | a `elem` ubs = (>)
            | otherwise    = (==)

check :: Stats (Int -> Bool) -> Stats Int -> Bool
check m = Map.foldr' (&&) True . Map.intersectionWith ($) m

trueSue :: [(String, Int)]
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

checker1 :: Stats (Int -> Bool)
checker1 = mkChecker [] [] trueSue

checker2 :: Stats (Int -> Bool)
checker2 = mkChecker ["cats", "trees"] ["pomeranians", "goldfish"] trueSue

main :: IO ()
main = do
    input <- readFile "input.txt"
    case parse input of
        Left err -> print err
        Right aunts -> do
            print $ fst . head $ filter (check checker1 . snd) aunts
            print $ fst . head $ filter (check checker2 . snd) aunts

{-
40
241
-}

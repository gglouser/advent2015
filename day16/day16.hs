import Control.Applicative
import qualified Text.Parsec as P

data Aunt = Aunt Int [(String, Int)] deriving Show

parse :: P.Parsec String () [Aunt]
parse = paunt `P.endBy` P.endOfLine
    where
        pnum = read <$> some P.digit
        pitem = (,) <$> (some P.lower <* P.string ": ") <*> pnum
        paunt = Aunt <$> (P.string "Sue " *> pnum <* P.string ": ")
                     <*> pitem `P.sepBy` P.string ", "

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

checkSue (Aunt _ attrs) = all (`elem` trueSue) attrs

checkSue2 (Aunt _ attrs) = all ch attrs
    where
        ch ("cats", n) = n > 7
        ch ("trees", n) = n > 3
        ch ("pomeranians", n) = n < 3
        ch ("goldfish", n) = n < 5
        ch attr = attr `elem` trueSue

main = do
    input <- readFile "input.txt"
    case P.parse parse "input.txt" input of
        Left err -> print err
        Right aunts -> do
            print $ length aunts
            print $ filter checkSue aunts
            print $ filter checkSue2 aunts

{-
40
241
-}

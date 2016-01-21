import Control.Applicative
import Data.Bits
import Data.Word
import qualified Data.Map.Lazy as Map
import qualified Text.Parsec as P

type Ident = String
type Signal = Word16
data Val = VIdent Ident | VNum Signal deriving Show
data Expr
    = NOT Val
    | AND Val Val
    | OR  Val Val
    | LSHIFT Val Int
    | RSHIFT Val Int
    | Simple Val
    deriving Show
data Wire = Wire Ident Expr deriving Show

type Parser = P.Parsec String ()

ptok :: Parser a -> Parser a
ptok t = t <* P.spaces

parseWire :: Parser Wire
parseWire = flip Wire <$> pexpr <* psym "->" <*> pid
    where
        pnum = ptok $ read <$> some P.digit
        pid  = ptok $ some P.lower
        psym = ptok . P.string
        pval = VIdent <$> pid
            <|> VNum . fromIntegral <$> pnum
        pexpr = NOT <$ psym "NOT" <*> pval
            <|> pval <**>
                (   flip AND    <$ psym "AND"    <*> pval
                <|> flip OR     <$ psym "OR"     <*> pval
                <|> flip LSHIFT <$ psym "LSHIFT" <*> pnum
                <|> flip RSHIFT <$ psym "RSHIFT" <*> pnum
                <|> pure Simple )

eval :: [Wire] -> Map.Map Ident Signal
eval sigs = m
    where
        m = Map.fromList $ map (\(Wire w e) -> (w, evalExpr e)) sigs
        evalExpr (NOT v) = complement $ evalVal v
        evalExpr (AND v1 v2) = evalVal v1 .&. evalVal v2
        evalExpr (OR  v1 v2) = evalVal v1 .|. evalVal v2
        evalExpr (LSHIFT v n) = evalVal v `shiftL` n
        evalExpr (RSHIFT v n) = evalVal v `shiftR` n
        evalExpr (Simple v) = evalVal v
        evalVal (VIdent i) = m Map.! i
        evalVal (VNum n) = n

wireUpdate :: Ident -> Signal -> [Wire] -> [Wire]
wireUpdate i v = map upd
    where
        upd w@(Wire i' _)
            | i' == i = Wire i $ Simple (VNum v)
            | otherwise = w

main :: IO ()
main = do
    input <- readFile "input.txt"
    case P.parse (many parseWire) "input.txt" input of
        Left err -> print err
        Right wires -> do
            let a = (eval wires) Map.! "a"
            putStrLn $ "part 1 - wire a: " ++ show a
            let wires' = wireUpdate "b" a wires
                a' = (eval wires') Map.! "a"
            putStrLn $ "part 2 - wire a: " ++ show a'

{-
part 1 - wire a: 3176
part 2 - wire a: 14710
-}

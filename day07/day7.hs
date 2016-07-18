import Control.Applicative
import Data.Bits
import Data.Word
import qualified Data.Map.Lazy as Map
import qualified Text.Parsec as P

type Ident = String
type Signal = Word16
data Val = VIdent Ident | VNum Signal deriving Show
data Expr
    = Signal Val
    | NOT Val
    | AND Val Val
    | OR  Val Val
    | LSHIFT Val Int
    | RSHIFT Val Int
    deriving Show

parse :: String -> Either P.ParseError (Map.Map Ident Expr)
parse = P.parse (Map.fromList <$> many wire) ""
    where
        wire = flip (,) <$> expr <* sym "->" <*> id
        expr = NOT <$ sym "NOT" <*> val
           <|> val <**>
                ( flip AND    <$ sym "AND"    <*> val
              <|> flip OR     <$ sym "OR"     <*> val
              <|> flip LSHIFT <$ sym "LSHIFT" <*> num
              <|> flip RSHIFT <$ sym "RSHIFT" <*> num
              <|> pure Signal )
        val = VIdent <$> id
          <|> VNum . fromIntegral <$> num
        sym = tok . P.string
        id  = tok (some P.lower)
        num = read <$> tok (some P.digit)

        tok :: P.Parsec String () a -> P.Parsec String () a
        tok t = t <* P.spaces

eval :: Map.Map Ident Expr -> Map.Map Ident Signal
eval sigs = m
    where
        m = Map.map evalExpr sigs
        evalExpr (Signal v) = evalVal v
        evalExpr (NOT v) = complement $ evalVal v
        evalExpr (AND v1 v2) = evalVal v1 .&. evalVal v2
        evalExpr (OR  v1 v2) = evalVal v1 .|. evalVal v2
        evalExpr (LSHIFT v n) = evalVal v `shiftL` n
        evalExpr (RSHIFT v n) = evalVal v `shiftR` n
        evalVal (VIdent i) = m Map.! i
        evalVal (VNum n) = n

wireUpdate :: Ident -> Signal -> Map.Map Ident Expr -> Map.Map Ident Expr
wireUpdate i v = Map.insert i (Signal (VNum v))

main :: IO ()
main = do
    input <- readFile "input.txt"
    case parse input of
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

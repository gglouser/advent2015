import Control.Applicative
import Control.Monad (forM_)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Text.Parsec as P

type Pos = (Int,Int)
data Action = TurnOn | TurnOff | Toggle deriving Show
data Command = Command Action Pos Pos deriving Show

gridW, gridH, gridSize :: Int
(gridW, gridH) = (1000, 1000)
gridSize = gridW * gridH

indices :: Pos -> Pos -> [Int]
indices (x0,y0) (x1,y1) = [gridW*y + x | y <- [y0..y1], x <- [x0..x1]]

runLights :: V.Unbox a => a -> (Action -> a -> a) -> [Command] -> V.Vector a
runLights init actionHandler cmds = V.create $ do
    v <- MV.replicate gridSize init
    forM_ cmds $ \(Command act p0 p1) ->
        forM_ (indices p0 p1) $
            MV.modify v (actionHandler act)
    return v

countPart1 :: [Command] -> Int
countPart1 = countOn . runLights False handler
    where
        countOn = V.foldl' (\n on -> if on then n+1 else n) 0
        handler TurnOn  = const True
        handler TurnOff = const False
        handler Toggle  = not

countPart2 :: [Command] -> Int
countPart2 = V.sum . runLights 0 handler
    where
        handler TurnOn  k = k + 1
        handler TurnOff k = max 0 (k - 1)
        handler Toggle  k = k + 2

parse :: String -> Either P.ParseError [Command]
parse = P.parse cmds ""
    where
        cmds = cmd `P.endBy` P.endOfLine
        cmd = Command <$> act <*> pos <* P.string " through " <*> pos
        act = TurnOn  <$ P.try (P.string "turn on ")
          <|> TurnOff <$ P.try (P.string "turn off ")
          <|> Toggle  <$ P.try (P.string "toggle ")
        pos = (,) <$> num <* P.char ',' <*> num
        num = read <$> some P.digit

main :: IO ()
main = do
    input <- readFile "input.txt"
    case parse input of
        Left err -> print err
        Right acts -> do
            print $ countPart1 acts
            print $ countPart2 acts

{-
400410
15343601
-}

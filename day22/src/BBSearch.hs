module BBSearch (
    BBResult(..),
    BBSearch,
    searchDFS,
    searchDFS2,
    searchPQ,
    searchFBFS
) where

import Data.Either (partitionEithers)
import qualified Data.Set as S
import Control.Monad.State

data BBResult s = BBResult {
    bestResult :: Maybe (Int, s),
    nodeCount :: !Int
    }

-- | Branch-and-bound search function signature.
type BBSearch a
    = (a -> Maybe Int)      -- ^ objective function
    -> (a -> Int)           -- ^ cost lower bound function
    -> (a -> [a])           -- ^ state branch function
    -> a                    -- ^ the initial state
    -> BBResult a

-- | Basic depth-first branch-and-bound search.
searchDFS :: BBSearch a
searchDFS objective bound branch s0 = go (BBResult Nothing 0) [s0]
    where
        go = foldl process
        process (BBResult x n) s =
            case objective s of
                Just b -> BBResult (minX x (Just (b, s))) n
                Nothing -> go (BBResult x (n+1)) (expand x s)
        expand (Just (i,_)) s = [s' | s' <- branch s, bound s' < i]
        expand Nothing s = branch s

-- | Depth-first branch-and-bound that remembers visited states so that
-- any given state is branched at most once.
searchDFS2 :: Ord a => BBSearch a
searchDFS2 objective bound branch s0 = fst $ go (BBResult Nothing 0, S.empty) [s0]
    where
        go = foldl process
        process r@(BBResult x n, visited) s
            | s `S.member` visited = r
            | otherwise =
                case objective s of
                    Just b -> (BBResult (minX x (Just (b, s))) n, S.insert s visited)
                    Nothing -> go (BBResult x (n+1), S.insert s visited) (expand x s)
        expand (Just (i,_)) s = [s' | s' <- branch s, bound s' < i]
        expand Nothing s = branch s

-- | Branch-and-bound using a priority queue.
searchPQ :: Ord a => BBSearch a
searchPQ objective bound branch s0 = go (BBResult Nothing 0) (S.singleton (maxBound, s0))
    where
        go r@(BBResult x n) ss =
            case S.minView ss of
                Nothing -> r
                Just ((_,s), ss') ->
                    case objective s of
                        Just b -> go (BBResult (minX x (Just (b,s))) n) ss'
                        Nothing -> go (BBResult x (n+1)) (S.union (expand x s) ss')
        expand (Just (i,_)) s = S.fromList [(b,s') | s' <- branch s, let b = bound s', b < i]
        expand Nothing s = S.fromList [(bound s', s') | s' <- branch s]

-- | \"Full-Breadth\" branch-and-bound search.
--
-- Like breadth-first search, but examines all nodes at depth d,
-- then moves on to depth d+1 and so on.
searchFBFS :: Ord a => BBSearch a
searchFBFS objective bound branch s0 = go (BBResult Nothing 0) [s0]
    where
        go r [] = r
        go (BBResult x n) ss =
            let (scores, ss') = partitionEithers . map check $ ss >>= branch
                newBest = foldl minX x scores
                newSS = S.toList . S.fromList $
                            case newBest of
                                Just (b,_) -> filter ((< b) . bound) ss'
                                Nothing -> ss'
            in go (BBResult newBest (n + length ss)) newSS

        check s = case objective s of
                    Just b -> Left (Just (b,s))
                    Nothing -> Right s

minByFst :: Ord a => (a,b) -> (a,b) -> (a,b)
minByFst x@(i,_) y@(j,_) = if j < i then y else x

-- I think this is the same as the monoid Option (ArgMin a b) from semigroups.
minX :: Ord a => Maybe (a,b) -> Maybe (a,b) -> Maybe (a,b)
minX Nothing y = y
minX x@(Just (i,_)) y@(Just (j,_)) = if j < i then y else x
minX x _ = x

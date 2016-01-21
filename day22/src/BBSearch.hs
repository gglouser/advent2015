module BBSearch (
    BBSearch,
    searchDFS,
    searchDFS2,
    searchPQ,
    searchFBFS
) where

import Data.Either (partitionEithers)
import qualified Data.Set as S

-- | Branch-and-bound search function signature.
type BBSearch a
    = (a -> Maybe Int)      -- ^ objective function
    -> (a -> Int)           -- ^ cost lower bound function
    -> (a -> [a])           -- ^ state branch function
    -> a                    -- ^ the initial state
    -> (Int, Maybe a, Int)

-- | Basic depth-first branch-and-bound search.
searchDFS :: BBSearch a
searchDFS objective bound branch s0 = go (maxBound, Nothing) [s0] 0
    where
        go (b,s) [] v = (b,s,v)
        go x (s:ss) v = case objective s of
                        Just b -> go (minByFst x (b, Just s)) ss $! v+1
                        Nothing -> go x (expand x s ++ ss) $! v+1
        expand (i,_) s = [s' | s' <- branch s, bound s' < i]

-- | Depth-first branch-and-bound that remembers visited states so that
-- any given state is branched at most once.
searchDFS2 :: Ord a => BBSearch a
searchDFS2 objective bound branch s0 = go (maxBound, Nothing) [s0] S.empty
    where
        go (b,s) [] visited = (b, s, S.size visited)
        go x (s:ss) visited | s `S.member` visited = go x ss visited
        go x (s:ss) visited =
            case objective s of
                Just b -> go (minByFst x (b, Just s)) ss (S.insert s visited)
                Nothing -> go x (expand x s ++ ss) (S.insert s visited)
        expand (i,_) s = [s' | s' <- branch s, bound s' < i]

-- | Branch-and-bound using a priority queue.
searchPQ :: Ord a => BBSearch a
searchPQ objective bound branch s0 = go (maxBound, Nothing) (S.singleton (maxBound, s0)) 0
    where
        go x@(q,r) ss v = case S.minView ss of
            Nothing -> (q,r,v)
            Just ((_,s), ss') ->
                case objective s of
                    Just b -> go (minByFst x (b, Just s)) ss' $! v+1
                    Nothing -> go x (S.union (expand x s) ss') $! v+1
        expand (i,_) s = S.fromList [(b,s') | s' <- branch s, let b = bound s', b < i]

-- | \"Full-Breadth\" branch-and-bound search.
--
-- Like breadth-first search, but examines all nodes at depth d,
-- then moves on to depth d+1 and so on.
searchFBFS :: Ord a => BBSearch a
searchFBFS objective bound branch s0 = go (maxBound, Nothing) [s0] 0
    where
        go (b,s) [] v = (b,s,v)
        go x ss v =
            let (scores, ss') = partitionEithers . map check $ ss >>= branch
                newBest = foldl minByFst x scores
                newSS = S.toList . S.fromList $ filter ((< fst newBest) . bound) ss'
            in go newBest newSS $! v + length ss

        check s = case objective s of
                    Just b -> Left (b, Just s)
                    Nothing -> Right s

minByFst :: Ord a => (a,b) -> (a,b) -> (a,b)
minByFst x@(i,_) y@(j,_) = if j < i then y else x

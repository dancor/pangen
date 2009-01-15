-- generic searching of infinite spaces with infinite pits in them..
-- (breadth searching plus heuristics)

-- f maps state to all poss contins

import qualified Data.Set as S

breadthSrch :: (a -> [a]) -> (a -> Bool) -> a -> Set a -> Set a
breadthSrch = breadthSrchSeen S.empty

breadthSrchSeen :: Set a -> (a -> [a]) -> (a -> Bool) -> a -> [a]
breadthSrchSeen seen nexts test initState =
  if initState `S.member` seen
    then []
    else
      (if test initState then [initState] else []) ++ breadthSrchSeen

-- assumes the nexts function returns a finite length list for all vals
breadthSrchSeen :: Set a -> (a -> [a]) -> (a -> Bool) -> a -> [a]
breadthSrchSeen seen nexts test curs =
  curs' = filter (`S.notMember` seen) curs
  cursOk = filter test curs'
  cursNext = concatMap nexts curs'

{-
-- wanted to not assume next makes finite, but need anyway for updating 'seen'
-- to terminate.  it's still possible that round robining will give better
-- average-case efficiency though..
breadthSrchSeen seen nexts test curs =
  breadthSrchSeen (seen `S.union`
  where
  cursOk = filter test curs
  cursNext = filter (`S.notMember` seen) . roundRob $ map nexts curs
-}

-- Combine lists round robin.
-- Note this prevents any single infinite list from occulding the others.
roundRob :: [[a]] -> [a]
roundRob ls = if null curRound then ls else curRound ++ roundRob ls' where
  (curRound, ls') = first concat . unzip $ map (splitAt 1) ls


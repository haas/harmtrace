module HarmTrace.HAnTree.PostProcess ( PPOption(..)
                                     , expandChordDurations
                                     , removePDPT, removeInsertions
                                     , mergeDelChords ) where

import HarmTrace.Base.MusicRep
import HarmTrace.Models.ChordTokens as CT
import HarmTrace.HAnTree.HAn
import HarmTrace.HAnTree.Tree

import Data.List(partition, find)
import Data.Maybe (isJust, fromJust)
-- import Debug.Trace

-- Parser stuff
import Text.ParserCombinators.UU.BasicInstances as PC


-- Optional post-processing operations   
data PPOption  = RemoveInsertions | RemovePDPT 
               | MergeDelChords   | ExpandChordDurations  deriving (Eq)

-- propagates the durations of the chords up into the tree
expandChordDurations :: Tree HAn -> Tree HAn
expandChordDurations (Node h [] a) = (Node h [] a)  where
expandChordDurations (Node h cs a) = (Node (setDur h d) cs' a)  where
     cs' = map expandChordDurations cs
     d   = sum $ map (getDur . getLabel) cs'

-- removes some nodes from the tree structure that are not important for
-- similarity estimation
removePDPT :: Tree HAn -> Tree HAn
removePDPT = removeBy (\l -> l `elem`  [(HAnFunc PD), (HAnFunc PT)])

-- Removes the HAn Nodes that were inserted by the parsing process
removeInsertions :: Tree HAn -> Tree HAn
removeInsertions = head . fst . remIns

remIns :: Tree HAn -> ([Tree HAn], Bool)
remIns l@(Node han [ ] _ ) = if isInserted han then ([],True) else ([l],False)
remIns   (Node han  cn pn) = ([Node han (concat trees) pn], False) where
  (trees,_ ) = unzip . filter (not . snd) $ map remIns cn
  
-- returns True if a HAn is Inserted
isInserted :: HAn -> Bool
isInserted (HAnChord (ChordToken _ _ _ CT.Inserted _ _)) = True
isInserted _                                             = False

--------------------------------------------------------------------------------
-- PostProcessing a Tree HAn with the chords deleted by the parser
--------------------------------------------------------------------------------

-- top level function for merging deleted chords
-- TODO: could be made to work on [ChordToken] instead of [ChordLabel]
mergeDelChords :: Key -> [Error Int] -> [ChordLabel] -> Tree HAn -> Tree HAn
mergeDelChords key pErr tok tree = 
  head $ mergeDelChords' key (groupNeighbours (filterErrorPos pErr tok)) [tree] 
  
-- N.B. there is a bug in this function: if the first chords is deleted 
-- it is not placed back because there is no chord in the tree before
-- the deleted chord.

-- merges the deleted chords back into the parsed Tree HAn
mergeDelChords' :: Key -> [[ChordLabel]] -> [Tree HAn] -> [Tree HAn]
mergeDelChords' _key [] tree = tree
mergeDelChords' _key _  []    = []
mergeDelChords'  key d (i@(Node (HAnChord c) _ _):ts)
  | status c == CT.Inserted = i : mergeDelChords' key d ts
  | isJust m  = i : (toDelHAn key $ fromJust m) ++ mergeDelChords' key d ts
  | otherwise = i : mergeDelChords' key d ts
  where m = find (\x -> (getLoc . last $ chords c) + 1 == (getLoc $ head x)) d
mergeDelChords'  key chrds (Node han cs pn : ts) =
  Node han (mergeDelChords' key chrds cs) pn : mergeDelChords' key chrds ts

-- transforms a (deleted) chord into a Tree HAn data type
toDelHAn :: Key -> [ChordLabel] -> [Tree HAn]
toDelHAn key m = map f m where
  f c@(Chord r sh _add _loc d) = (Node (HAnChord
    (ChordToken (toScaleDegree key r) (toClassType c) [c] CT.Deleted 1 d))
    [] Nothing)

-- returns the deleted chords, given a list of errors and the input tokes
filterErrorPos :: [Error Int] -> [Chord a] -> [Chord a]
filterErrorPos e c = filter (\x -> getLoc x `elem` dels) chrds ++ cDelsAtEnd
  where
  (delsAtEnd, dels) = partition (== (-1)) . map gPos $ filter f e
  (chrds,cDelsAtEnd) = splitAt (length c - length delsAtEnd) c
  gPos (PC.Inserted _ p _) = p
  gPos (PC.Deleted  _ p _) = p
  gPos (DeletedAtEnd _)    = (-1)
  gPos (Replaced _ _ p _)  = p
  f    (PC.Inserted _ _ _) = False
  f    (PC.Deleted  _ _ _) = True
  f    (DeletedAtEnd _)    = True
  f    (Replaced _ _ _ _)  = False

-- groups the deleted chord tokens that are neighbours, if we were not
-- grouping chords but Integers, a result could look like:
-- groupNeighbours [1,2,7,8,9,11,13,16,17] = [[1,2],[7,8,9],[11],[13],[16,17]]
groupNeighbours :: [Chord a] -> [[Chord a]]
groupNeighbours []     = []
groupNeighbours (x:xs) = let (grp,tl) = get x xs in grp : groupNeighbours tl
-- splits a list into a list with neighbours and a tail
get :: Chord a -> [Chord a] -> ([Chord a],[Chord a])
get a l@[]  = ([a],l)
get a l@(b:cs)
  | (getLoc a) + 1 == getLoc b = (a:bs,cs')
  | otherwise  = ([a],l) where (bs,cs') = get b cs
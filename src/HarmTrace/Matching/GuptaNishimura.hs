{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
module HarmTrace.Matching.GuptaNishimura ( getLCES     , getLCESsize
                                         , getLCESdepth, getLCESsim) 
where

--------------------------------------------------------------------------------
-- Finding the Largest Common Embedable Subtrees (LCES)
-- Based on: Gupta, A. and Nishimura, N. (1998) Finding largest subtrees and 
--           smallest supertrees, Algorithmica, 21(2), p. 183--210
-- author: Bas de Haas
-------------------------------------------------------------------------------- 

import Data.Ord
import Data.Maybe
import Prelude     hiding (length, last)
import Data.Vector hiding ((!), last)
import qualified Data.List as L

import HarmTrace.HAnTree.Tree
import HarmTrace.HAnTree.HAn
import HarmTrace.Matching.Sim

-------------------------------------------------------------------------------- 
-- Top Level LCES function
-------------------------------------------------------------------------------- 

getLCESsim :: Tree HAn -> Tree HAn -> Float
getLCESsim ta tb = let match    = fromIntegral . snd $ getLCES ta tb
                       selfSimA = sim ta ta * cumDur ta
                       selfSimB = sim tb tb * cumDur tb
                   in (match * match) / fromIntegral (selfSimA * selfSimB)

getLCESsize :: Tree HAn -> Tree HAn -> Float
getLCESsize ta tb = let match = fromIntegral . sizeF . fst $ getLCES ta tb
                    in (match * match) / fromIntegral (size ta * size tb)

-- Check ismir09 implementation
getLCESdepth :: Tree HAn -> Tree HAn -> Float
getLCESdepth ta tb = let match = avgDepthF . fst $ getLCES ta tb
                     in (match * match) / (avgDepth ta * avgDepth tb) 

-- Top level function that returns the largest common embedable subtree
-- of two trees
getLCES :: Tree HAn -> Tree HAn -> ([Tree HAn], Int)
getLCES ta tb = (matchToTree ta (L.map fst (L.reverse m)),w) where 
  (LCES m w)  = last . last $ lces ta tb

nonMatchPenal :: Int
nonMatchPenal = 2  
  
-------------------------------------------------------------------------------- 
-- LCES calculation
--------------------------------------------------------------------------------   
  
-- calculates the largest labeled common embeddable subtree
lces :: (Sim t, GetDur t) => Tree t -> Tree t -> Vector (Vector LCES)
lces ta tb = n where
  a = fromList (pot ta)
  b = fromList (pot tb)
  maxi :: Int -> [Int] -> LCES
  {-# INLINE maxi  #-}  
  maxi _ [] = emptyLCES  
  maxi i cb = (n!i) ! (L.maximumBy (comparing (\j -> getWeight $ ((n!i)!j))) cb)
  maxj :: [Int] -> Int -> LCES
  {-# INLINE maxj  #-}  
  maxj [] _ = emptyLCES  
  maxj ca j = (n ! (L.maximumBy (comparing (\i -> getWeight $ ((n!i)!j))) ca))!j
  recur 0 0 = if sim (getLabel (a ! 0)) (getLabel (b ! 0)) > 0
              then LCES [(0,0)] (durSim (getLabel (a ! 0)) (getLabel (b ! 0)))
              else emptyLCES
  recur i j = findBestMatch (sim labi labj) 
                            (min (getDur labi) (getDur labj)) i j mc mi mj where  
    mi  = maxi i  (getChildPns (b ! j))
    mj  = maxj    (getChildPns (a ! i)) j
    mc  = wbMatch (getChild (a ! i)) (getChild $ b ! j) n
    !labi = getLabel (a!i) 
    !labj = getLabel (b!j)
  n = generate (length a) (generate (length b) . recur)  
      
-- returns the best matching candidate, given the previous candidates, the
-- bipartite matching. The function depends on wheter the currend nodes 
-- match and whether, in that case, one of the current nodes is not allready 
-- matched
findBestMatch :: Int -> Int -> Int -> Int -> LCES -> LCES -> LCES -> LCES 
{-# INLINE findBestMatch  #-} 
findBestMatch simv dur i j a b c
  | simv <= 0 = (LCES mf (max (wf - (nonMatchPenal * dur)) 0 ))
  | otherwise = if isFree first i j       then (LCES ((i,j):mf) (wf+(dur*simv))) 
                else if wf /= ws          then first           
                else if isFree second i j then (LCES ((i,j):ms) (ws+(dur*simv)))
                else if wf /= wt          then first
                else if isFree second i j then (LCES ((i,j):mt) (wt+(dur*simv)))
                else first where
    (first@(LCES mf wf) :second@(LCES ms ws) :(LCES mt wt) :[]) = mySort [a,b,c]
          
--------------------------------------------------------------------------------
-- Weighted Plannar Matching of a Bipartite Graph
--------------------------------------------------------------------------------   
  
-- returns the actual planar weighted bipartite matchings. n should contain 
-- the weights of the edge between a[i] and b[j]  
wbMatch   :: [Tree t] -> [Tree t] ->  Vector (Vector LCES) ->  LCES
{-# INLINE wbMatch  #-} 
wbMatch _ []  _ = emptyLCES
wbMatch [] _  _ = emptyLCES
wbMatch a b n = last $ last m where 
  -- returns a previously matched subtree 
  subTree :: Int -> Int -> LCES 
  {-# INLINE subTree  #-}  
  subTree i j = (n ! (fromJust . getPn $ a!!i)) ! (fromJust . getPn $ b!!j)
  -- this is the actual core recursive definintion of the algorithm
  match, fill :: Int -> Int -> LCES 
  match i j = L.maximumBy (comparing getWeight) [maxPrv, minPrv, diagM] where
    s         = subTree i j
    !hasMatch = getWeight s > 0
    maxPrv    = if not hasMatch                    then (m ! (i-1)) ! j
                else if isFree ((m!(i-1)) ! j) i j then merge s ((m!(i-1)) ! j)
                else ((m ! (i-1)) ! j)
    minPrv    = if not hasMatch                    then (m ! i) ! (j-1)
                else if isFree ((m!i) ! (j-1)) i j then merge s ((m!i) ! (j-1)) 
                else ((m ! i) ! (j-1))
    diagM     = merge s ((m ! (i-1)) ! (j-1)) 
  fill 0 0 = subTree 0 0
  fill 0 j = if getWeight (subTree 0 j) > getWeight ((m ! 0) ! (j-1)) 
             then subTree 0 j else (m ! 0) ! (j-1)
  fill i 0 = if getWeight (subTree i 0) > getWeight ((m ! (i-1)) ! 0) 
             then subTree i 0 else ((m ! (i-1)) ! 0)
  fill i j = match i j             
  m = generate (L.length a) (generate (L.length b) . fill)   
     
--------------------------------------------------------------------------------
-- Some LCES helper functions
-------------------------------------------------------------------------------- 

data LCES =  LCES  ![(Int, Int)] !Int 

getWeight :: LCES -> Int
{-# INLINE getWeight #-}
getWeight (LCES _ w) = w

-- getMatch :: LCES -> [(Int, Int)]
-- getMatch (LCES m _) = m

durSim :: (Sim a, GetDur a) => a -> a -> Int
durSim a b = (sim a b) * (min (getDur a) (getDur b))

emptyLCES :: LCES
{-# INLINE emptyLCES #-}
emptyLCES =  LCES [] 0  

(!) :: Vector a -> Int -> a
{-# INLINE (!) #-}
(!) = unsafeIndex

last :: Vector a -> a
{-# INLINE last #-}
last = unsafeLast

cumDur :: (GetDur a) => Tree a -> Int
cumDur a = (getDur $ getLabel a) + (L.sum $ L.map cumDur (getChild a))

-- checks if the previously calculated optimal solution does not 
-- contain the indices i and j in a and b, resepectivly
isFree :: LCES -> Int -> Int -> Bool
{-# INLINE isFree  #-}
isFree (LCES []                 _) _ _ = True
isFree (LCES ((previ, prevj):_) _) i j = ( i > previ && j > prevj)

-- mergest two lists with matches
merge :: LCES -> LCES -> LCES
{-# INLINE merge  #-}
merge (LCES a wa) (LCES b wb) = LCES (a L.++ b) (wa + wb)

-- this sorting routine makes quite a large difference in runtime performance!
mySort :: [LCES] -> [LCES]
{-# INLINE mySort  #-}
mySort [a,b,c] = case (x >= y, y >= z, x >= z) of
                   (True , True , True ) -> [a,b,c]
                   (True , False, True ) -> [a,c,b]
                   (True , False, False) -> [c,a,b]
                   (False, True , True ) -> [b,a,c]
                   (False, True , False) -> [b,c,a]
                   (False, False, False) -> [c,b,a]
                   _ -> error "mySort: impossible"
  where !x = getWeight a
        !y = getWeight b
        !z = getWeight c
mySort _ = error "mySort: unexpected argument"

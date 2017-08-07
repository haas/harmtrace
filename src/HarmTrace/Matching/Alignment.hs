{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module HarmTrace.Matching.Alignment ( alignChordLab, pPrintV, getAlignDist
                                    , getHAnDist, alignHAnChord
                                    -- , getDownRight, wbMatchF, align, Sim(..)
                                    -- , collectMatch
                                    ) where

import HarmTrace.Base.MusicRep
import HarmTrace.Matching.SimpleChord
import HarmTrace.Matching.HChord
import HarmTrace.Matching.Sim
import HarmTrace.HAnTree.HAn 
import HarmTrace.HAnTree.Tree

import Prelude hiding (map, length, head, last, mapM_, max)

import Data.Vector hiding ((!), (++))
import qualified Data.List as L

-- import Debug.Trace
{-

Matching notes:
===============
** Normalisation ( sim * sim ) / (maxsim a * maxsim b) helps in practically 
   all cases.
** The sampling in general has a large effect on matching speed, and a small 
   effect on retrieval performance. In all observed cases using no sampling 
   performs (slightly) better than not using sampling. The sample rate herein 
   also has an effect: using normal integer division (`div`) deletes chords 
   with a beat length of 1, which decreases retrieval performance. It is
   better to use a `div1` that also includes the chords with a duration of
   beat (see SimpleChord.myDiv)
** The mis-match penalty should be -2 > -1 < 0: -1 seems to be optimal 
   (= insertion/deletion)
** Use very "conservative" similarity measures (not many things are similar) 
** Using a ChordType instead of just major/minor improves results
** Using the HAnTrans information improves the similarity estimation
** using only the information of the model (HAnTrans and HAnFunc) performs
   worse than using the root and chord type
** Separating transformations (HAnTrans), i.e. Tritone substitutions, dimchord
   transformations etc., from preparations (HAnPrep), i.e. secondary dominants,
   diatonic chains etc., improves results. This is probably due to that 
   previously a transformation could "override" a preparations because only
   one HAnTrans node was stored (the lowest one in the tree). 
** adding similarity between various different preparations DiatV == SecDom
   improves similarity. This makes sense because both involve fifth jumps   

-}

--------------------------------------------------------------------------------
-- Baseline chord label alignment (no model)
--------------------------------------------------------------------------------    

-- returns a similarity/distance value  
getAlignDist :: Key -> Key -> [ChordLabel] -> [ChordLabel] -> Float
getAlignDist ka kb ta tb = let (_match, dist, _tab) = alignChordLab ka kb ta tb
                           in dist 

                            
alignChordLab :: Key -> Key -> [ChordLabel] -> [ChordLabel] 
              -> ([SimChord], Float, Vector (Vector Int))
alignChordLab ka kb ta tb = (fst $ matchToSeq match ta' tb', dis, tab) where
  (match, weight, tab) = --trace ("ta: " ++ show ta'++ "\ntb: "++ show tb') 
                       align (-2) ta' tb' 
  dis =   fromIntegral (weight * weight) 
        / fromIntegral (maxSim ta' * maxSim tb')
  ta' = L.concatMap (toSimChords . toChordDegree ka) ta
  tb' = L.concatMap (toSimChords . toChordDegree kb) tb

--------------------------------------------------------------------------------
-- HAn Chord alignment
--------------------------------------------------------------------------------    

-- returns a similarity/distance value  
getHAnDist :: Tree HAn -> Tree HAn -> Float
getHAnDist ta tb = let (_match, dist, _tab) = alignHAnChord ta tb in dist 
                            
alignHAnChord :: Tree HAn -> Tree HAn -> ([HChord], Float, Vector (Vector Int))
alignHAnChord ta tb = 
  -- trace ("ta: " ++ show ta'++ "\ntb: "++ show tb' ++ "\nsim: "++ show dis)
  (fst $ matchToSeq match ta' tb', dis, tab) where
    (match, weight, tab) = align (-2) ta' tb' 
    dis =   fromIntegral (weight * weight) 
          / fromIntegral (maxSim ta' * maxSim tb')
    ta' = toHChords ta 
    tb' = toHChords tb 
  
-- creates an alignment and returns the list of matches, the distance, and
-- the alignment table. The first argument is the insertion/deletion
-- penalty (should be a negative value).
align :: Sim a => Int -> [a] -> [a] -> ([(Int,Int)], Int, Vector (Vector Int))
align _ _  [] = ([],0,empty)
align _ [] _  = ([],0,empty)
align inDel a b = (cm, getDownRight t,t) where
  t  = wbMatchF inDel a b
  cm = toList (collectMatch t)
    
wbMatchF :: Sim a => Int -> [a] -> [a] -> Vector (Vector Int)
wbMatchF _ _ []  = empty 
wbMatchF _ [] _  = empty
wbMatchF inDel a' b' = m where
  a  = fromList a' 
  b  = fromList b'       
  match, fill :: Int -> Int -> Int
  {-# INLINE fill  #-}
  match i j = sim (a ! i) (b ! j)  
  -- this is the actual core recursive definintion of the algorithm
  fill 0 0 = max  (match 0 0) 0
  fill 0 j = max0 (((m ! 0   ) !(j-1)) + inDel) (match 0 j) 
  fill i 0 = max0 (((m !(i-1)) ! 0   ) + inDel) (match i 0)  
  fill i j = max3 (((m !(i-1)) ! j   ) + inDel) 
                  (((m !(i-1)) !(j-1)) + match i j) 
                  (((m ! i)    !(j-1)) + inDel)
  m = generate (length a) (generate (length b) . fill)


--------------------------------------------------------------------------------
-- Getting the alignment out of the table
-------------------------------------------------------------------------------- 

collectMatch :: Vector (Vector Int) -> Vector (Int,Int)
collectMatch a = fromList $ collect a (length a -1, length (head a) -1) []
collect :: (Ord b, Num b) => Vector (Vector b) -> (Int, Int) -> [(Int, Int)] 
        -> [(Int, Int)]
collect a c@(0,0) m = if (a!0)!0 > 0 then c : m else m
collect a c@(i,0) m = if (a!i)!0 > (a!(i-1))! 0 
                                     then c : m else collect a (i-1,0) m
collect a c@(0,j) m = if (a!0)!j > (a!0    )!(j-1) 
                                     then c : m else collect a (0,j-1) m
collect a c@(i,j) m 
  | (a ! i) ! j > snd o = collect a (fst o) (c : m)
  | otherwise               = collect a (fst o) m where 
      o = realMax3 ((i-1,j)  , (a !(i-1)) ! j   )
                   ((i-1,j-1), (a !(i-1)) !(j-1))
                   ((i,j-1)  , (a ! i   ) !(j-1))

realMax3 :: (Ord a) => (t, a) -> (t, a) -> (t, a) -> (t, a)    
realMax3 w nw n = maxByWeight nw (maxByWeight w n) where
  maxByWeight :: Ord a => (t,a) -> (t,a) -> (t,a)                                        
  maxByWeight a@(_,wa) b@(_,wb) = if wa > wb then a else b 

--------------------------------------------------------------------------------
-- Some LCES helper functions
-------------------------------------------------------------------------------- 

matchToSeq :: [(Int,Int)] -> [a] -> [a] -> ([a],[a])
matchToSeq mat aOrg bOrg = (f aMat aOrg, f bMat bOrg) where
  f m o = fst . L.unzip $ L.filter (\(_,x) -> x `L.elem` m) (L.zip o [0..]) 
  (aMat, bMat) = L.unzip mat

(!) :: Vector a -> Int -> a
{-# INLINE (!) #-}
(!) = unsafeIndex

max3 :: (Ord a, Num a) => a -> a -> a -> a
{-# INLINE max3 #-}
max3 a b c = max a (max0 b c)

max0  :: (Ord a, Num a) => a -> a -> a 
{-# INLINE max0 #-}
max0 a b = max a (max b 0)
-- max3' w nw n = if n > nw then n else max nw w -- not correct yet

max :: (Ord a, Num a) => a -> a -> a 
{-# INLINE max #-}
max x y = if x <= y then y else x

getDownRight :: Vector (Vector a) -> a
getDownRight n = last (last n) 

-- pretty prints a 2 dimensional vector in a readable format
pPrintV :: Show a => Vector (Vector a) -> IO ()
pPrintV = mapM_ printLn where
  printLn :: Show a => Vector a -> IO()
  printLn v = do mapM_ (\x -> putStr (show x ++ " ")) v ; putChar '\n'


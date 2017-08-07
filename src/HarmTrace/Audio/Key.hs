{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall         #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Audio.Key
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Key-finding from musical audio.
--------------------------------------------------------------------------------

module HarmTrace.Audio.Key (getBeatSyncKeyFromChroma) where

import HarmTrace.Base.MusicTime
import HarmTrace.Audio.Statistical (groupMinSize, mode)
import HarmTrace.Audio.ChromaKey ( beatSyncKeyStrength, keyMap)
import HarmTrace.Base.MusicRep
import Constants (minModulationLength, modulationPenalty)

import Prelude as P hiding ( map, length, head, last, mapM, mapM_, max
                           , maximum, reverse, tail, null, concatMap )

-- N.B. Vector inside this module refers to a different type than Vector
--      in the HarmTrace.Audio.ChromaChord module
import Data.Vector as V 
import qualified Data.List as L
import Control.Arrow ((&&&))
import Data.Ord (comparing)

--------------------------------------------------------------------------------
-- Chroma key estimation
--------------------------------------------------------------------------------

-- | Returns a 'Key' key assignment, given 'Beat' and chroma information.
-- N.B. The beats and chroma do not have to be synchronised, they will be
-- sychronised in the process of finding the key 
-- (see 'Harmtrace.Audio.ChromaKey' and 'Harmtrace.Audio.ChromaChord.BeatSync')
getBeatSyncKeyFromChroma :: AudioFeat -> [TimedData Key]
getBeatSyncKeyFromChroma (AudioFeat _cs bt key _id) = 
  mergeAndTimeStamp fstBeat syncbt
  . groupKeys . getKeyFromTable . selectKey $ beatSyncKeyStrength bt key where

      -- This synchronisation is ugly, but necessary... 
      -- keyStr = beatSyncKeyStrength bt key 
      fstBeat  = BarTime  0                    (prevBeat . beat . L.head $ bt)
      lstBeat  = BarTime (time . L.last $ key) (nextBeat . beat . L.last  $ bt)
      syncbt   = L.takeWhile (< lstBeat) bt L.++ [lstBeat]
              
      -- | Given a list of beats, a list of grouped data items, and a merging function
      -- 'mergeAndTimeStamp' returns a list of 'BeatTimedData'. Before wrapping the
      -- the grouped data items, e.g. chord candidates, the list is reduced by the 
      -- provided merging function.
      mergeAndTimeStamp :: BarTime -> [BarTime] -> [[a]] -> [TimedData a]
      mergeAndTimeStamp  _                 []    []       = []
      mergeAndTimeStamp on beats (x : xs) = 
        let (off : rest) = L.drop (L.length x -1) beats
        in  TimedData (L.head x) [on, off] : mergeAndTimeStamp off rest xs
      mergeAndTimeStamp _ _ _ = error "mergeAndTimeStamp: asynchronous beats and data"
              
              
-- selectKey :: [BarTime] -> [ChordinoLine] ->  Vector (Vector (Int, NumData))
selectKey :: [TimedData [NumData]] ->  Vector (Vector (Int, NumData))
selectKey  []  = empty 
-- selectKey  []  _key = empty
selectKey  key = k where
  -- start by calculating the beat synchronised key strenght for all 
  -- 24 keys (ordered by HarmTrace.Audio.ChromaKey.keyMap)
  m :: Vector (Vector NumData)
  m = fromList . L.map fromList $ dropTimed key
  
  -- calculate for every beat the maximum key (the index) and the 
  -- profile correlation (snd)
  maxima :: Vector (Int, NumData)
  {-# INLINE maxima  #-}
  -- maxima = map (\x -> (maxIndex x, maximum x)) m
  maxima = map (maxIndex &&& maximum) m
  
  -- we fill a beat x 24 table and store the cumulative key strength.
  -- we can chose to stay in the current key or we can modulate which is
  -- penalised by modulationPenalty, we also store the index so we can follow
  -- the path back to the first beat
  fill :: Int -> Int -> (Int, NumData)
  {-# INLINE fill  #-}
  fill 0 j = (j, (m!0)!j)
  fill i j = let (mj, mv) = maxima!i -- current max
                 noModul  = (j , snd ((k!(i-1))!j) + ((m!i)!j))
                 modul    = (mj, snd ((k!(i-1))!j) + mv - modulationPenalty)
                 -- Reviewer #3 writes that the line above is incorrect. He/She 
                 -- suggests the line below:
                 -- modul    = (mj, (snd ((k!(i-1))!mj)) +mv -modulationPenalty)
                 -- Reviewer #3 is wrong: k!(i-1))!j represents the cumulative
                 -- key-strength *up to* the modulation at k!i from k!i on, the
                 -- new key, represehtned by mj, should be used to calculate
                 -- the cumulative key strength at the next beat. 
                 -- However, we should evaluate the key finding independently. 
             in max2 modul noModul 

  k = generate (length m) (generate 24 . fill)

max2 :: (Int, NumData) -> (Int, NumData) -> (Int, NumData)
{-# INLINE max2  #-}
max2 t1@(_, s1) t2@(_, s2) = if s1 > s2 then t1 else t2

-- given the cumulative key strength tabel, this function returns the beat-wise
-- key assignments
getKeyFromTable :: Vector (Vector (Int, NumData)) -> [Key]
getKeyFromTable k = L.map (keyMap !!) (L.reverse yek) where
  yek   = collectMax (getMax $ last k) (reverse k)
  -- given the table calulated with selectKey, this function calculates 
  -- the actual key assignment for every beat
  collectMax :: Int -> Vector (Vector (Int, NumData)) -> [Int]
  collectMax startj l 
    | null l     = []
    | otherwise  = fst (head l ! startj) : collectMax (getMax $ head l) (tail l) 
        
  getMax :: Vector (Int, NumData) -> Int
  getMax = fst . maximumBy (comparing snd) 

-- debugging:
-- printKeyTable :: [BarTime] -> [ChordinoLine] -> IO ()
-- printKeyTable bts chrm = 
  -- let showLn :: Vector (Int, NumData) -> IO ()
      -- showLn x = do mapM_ (\(i,f) -> putStr (printf "(%d, %.2f)" i f)) x
                    -- putStr "\n"
  -- in mapM_ showLn $ selectKey bts chrm 

-- naiveBeatSyncKey :: BarTimeTrackData -> [ChordinoLine] -> [Key]
-- naiveBeatSyncKey bts key = 
  -- L.map (((!!) keyMap) . maxListIndex) $ beatSyncKeyStrenth bts key

--------------------------------------------------------------------------------
-- key strengthpParsing
--------------------------------------------------------------------------------

-- Smooths and groups the key changes. Given a list of beat-wise key assignments
-- the similar keys are grouped, but a new group, i.e. modulation, has to have 
-- the minimum size of 16 beats. If this is not the case, the key assignments
-- will be replaced by the global key.
-- See HarmTrace.Audio.Statistical.groupMinSize
groupKeys :: [Key] -> [[Key]]
groupKeys ks = L.group . L.concat $
               groupMinSize minModulationLength (getGlobalKey ks) ks

-- Selects the key that is most prominent.
getGlobalKey :: [Key] -> Key
getGlobalKey = mode


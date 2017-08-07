
--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Audio.ChromaKey
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Low-processing of chroma features for key-finding
--------------------------------------------------------------------------------

module HarmTrace.Audio.ChromaKey ( beatSyncKeyStrength, keyMap ) where

import HarmTrace.Audio.ChromaChord ( beatSync
                                   , meanBeatSyncVectors
                                   )

import Data.Vector                    ( Vector, fromList )
import HarmTrace.Base.MusicTime
import HarmTrace.Base.MusicRep
-- import Statistics.Correlation         ( pearson )
--------------------------------------------------------------------------------
-- Chroma key estimation
--------------------------------------------------------------------------------

-- TODO implement a pearson correlation coefficient
pearson = undefined

-- | Calculates the beat synchronised key strenght for all
-- 24 keys (ordered by 'KeyMap').
beatSyncKeyStrength :: BarTimeTrackData -> ChordinoData -> [TimedData [NumData]]
beatSyncKeyStrength bts key =
  map matchKeyProfiles . meanBeatSyncVectors $ beatSync bts key where

  -- canMerge :: Beat -> Beat -> Bool
  -- canMerge One Two   = True
  -- canMerge One Three = True
  -- canMerge One Four  = True
  -- canMerge _   _     = False


nanToZero :: Double -> NumData
nanToZero n = if isNaN n then 0 else n

matchKeyProfiles :: TimedData (Vector NumData) -> TimedData [NumData]
-- matchKeyProfiles chroma = map (\x -> pnorm PNorm2 (chroma - x)) allKeyProfiles
-- matchKeyProfiles crm = map (nanToZero . GSL.correlation crm) allKeyProfiles
-- matchKeyProfiles crm = map (nanToZero . correlation crm) allKeyProfiles
matchKeyProfiles = fmap (\x -> map (nanToZero . pearson x) allKeyProfiles)

allKeyProfiles :: [Vector Double]
allKeyProfiles = map (fromList . keyToProfile) keyMap

keyToProfile :: Key -> [Double]
keyToProfile (Key root m) = reverseShift (toSemitone root) (selectProfile m)
  where reverseShift :: Int -> [a] -> [a]
        reverseShift p l = b ++ a where (a,b) = splitAt (length l - p) l

selectProfile :: Mode -> [Double]
selectProfile MajMode = temperleyProfCMaj
selectProfile MinMode = temperleyProfCMin

-- krumhanslProfCMaj, krumhanslProfCMin :: [NumData]
-- krumhanslProfCMaj =
  -- [6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88]
-- krumhanslProfCMin =
  -- [6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17]

temperleyProfCMaj, temperleyProfCMin :: [Double]
temperleyProfCMaj =
  [5.0,  2.0,  3.5,  2.0,  4.5,  4.0,  2.0,  4.5,  2.0,  3.5,  1.5,  4.0 ]
temperleyProfCMin =
  [5.0,  2.0,  3.5,  4.5,  2.0,  4.0,  2.0,  4.5,  3.5,  2.0,  1.5,  4.0 ]
--------------------------------------------------------------------------------
-- key strength Matrix Computations
--------------------------------------------------------------------------------

-- | A key chroma map using a circle of fifths based ordering.
keyMap :: [Key]
keyMap =  [ Key (Note (Just Sh) F) MajMode -- "F#" 6
          , Key (Note Nothing   B) MajMode -- "B"  11
          , Key (Note Nothing   E) MajMode -- "E"  4
          , Key (Note Nothing   A) MajMode -- "A"  9
          , Key (Note Nothing   D) MajMode -- "D"  2
          , Key (Note Nothing   G) MajMode -- "G"  7
          , Key (Note Nothing   C) MajMode -- "C"  0
          , Key (Note Nothing   F) MajMode -- "F"  5
          , Key (Note (Just Fl) B) MajMode -- "Bb" 10
          , Key (Note (Just Fl) E) MajMode -- "Eb" 3
          , Key (Note (Just Fl) A) MajMode -- "Ab" 8
          , Key (Note (Just Fl) D) MajMode -- "Db" 1
          , Key (Note (Just Fl) E) MinMode -- "Ebm" 3
          , Key (Note (Just Sh) G) MinMode -- "G#m" 8
          , Key (Note (Just Sh) C) MinMode -- "C#m" 1
          , Key (Note (Just Sh) F) MinMode -- "F#m" 6
          , Key (Note Nothing   B) MinMode -- "Bm"  11
          , Key (Note Nothing   E) MinMode -- "Em"  4
          , Key (Note Nothing   A) MinMode -- "Am"  9
          , Key (Note Nothing   D) MinMode -- "Dm"  2
          , Key (Note Nothing   G) MinMode -- "Gm"  7
          , Key (Note Nothing   C) MinMode -- "Cm"  0
          , Key (Note Nothing   F) MinMode -- "Fm"  5
          , Key (Note (Just Fl) B) MinMode -- "Bbm" 10
          ]

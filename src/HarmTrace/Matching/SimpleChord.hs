module HarmTrace.Matching.SimpleChord (SimChord, Sim, toSimChords) where

import HarmTrace.Base.MusicRep
import HarmTrace.Matching.Sim

-- represents a  very simple chord, only major and minor and a root scaledegree
data SimChord = SimChord !Int  -- I = 0, IIb = 1 ... VII = 11
                         !Mode -- maj = True, min = False
  
instance Sim SimChord where
  {-# INLINE sim #-}
  sim (SimChord r sh) (SimChord r2 sh2) -- = simInt r r2 + simInt sh sh2
    | r == r2 && sh == sh2 =  4
    | otherwise            = -1
    
instance Show SimChord where
  show (SimChord r sh) = show (toScaleDegree (Key (Note Nothing C) MajMode) 
                                             (toRoot r))
                         ++ if sh == MajMode then "" else "m"
    
toSimChords :: ChordDegree -> [SimChord]
toSimChords c = replicate (duration c) (SimChord (toSemitone . chordRoot $ c) m) 
  
  where m = case toTriad c of
              NoTriad -> MinMode -- this is odd, but this is how it was....
              t       -> toMode t
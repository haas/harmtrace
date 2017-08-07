module HarmTrace.Matching.HChord (HChord, Sim, toHChords) where

import HarmTrace.Base.MusicRep
import HarmTrace.Models.ChordTokens
import HarmTrace.Matching.Sim 
import HarmTrace.HAnTree.HAn
import HarmTrace.HAnTree.Tree

-- represents a  very simple chord, only major and minor and a root scaledegree
data HChord = HChord { deg  :: !Int       -- I = 0, IIb = 1 ... VII = 11
                     , clss :: !ClassType -- MajClass | MinClass | DomClass | ..
                     , func :: !HFunc
                     , prep :: !Prep
                     , trns :: !Trans}

instance Sim HChord where
  {-# INLINE sim #-}
  sim (HChord r ct _fc pr tr) (HChord r2 ct2 _fc2 pr2 tr2) 
    | r == r2 && ct == ct2 = 2 + sim pr pr2 + sim tr tr2
    | otherwise = -1
    
instance Show HChord where
  show (HChord r ct fc pr tr) =        show fc ++ ':' : show pr ++ ':' : show tr
    ++ ':' : show (toScaleDegree (Key (Note Nothing C) MajMode) (toRoot r)) 
    ++ show ct

toHChords :: Tree HAn -> [HChord]
toHChords t = getHAn undefinedHChord t

-- getHAn also samples/replicates the chords based on their duration in beats
getHAn :: HChord -> Tree HAn -> [HChord]
getHAn c (Node h@(HAnChord ct) [] _) -- there might be inserted chords
  | null (chords ct)   = []          -- ignore them in the matching process
  | otherwise          = let c'  = update c h 
                                     -- ignore func when the chord is deleted
                             c'' = if status ct == Deleted 
                                   then c' { trns = NoTrans } else c'
                         in  replicate (dur ct)    c''
                         -- in replicate ((dur ct) `div1` 2) c''
getHAn c (Node h cs _) = let c' = update c h in concatMap (getHAn c') cs

update :: HChord -> HAn -> HChord
update hc (HAn    _ _) = hc
update hc (HAnFunc  f) = hc { func = f }
update hc (HAnTrans t) = hc { trns = t }
update hc (HAnPrep  p) = hc { prep = p }
update hc (HAnChord c) = hc { deg  = toSemitone $ root c
                            , clss = classType c }

undefinedHChord :: HChord
undefinedHChord =  HChord (-1 :: Int)  (MajClass :: ClassType)
                          (P :: HFunc) NoPrep NoTrans

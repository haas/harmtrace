{-# LANGUAGE TemplateHaskell                #-}
{-# LANGUAGE EmptyDataDecls                 #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE GADTs                          #-}
{-# LANGUAGE DeriveGeneric                  #-}

module HarmTrace.HAnTree.HAn where 

import HarmTrace.Base.MusicRep
import HarmTrace.Models.ChordTokens

import Control.DeepSeq
import Data.Binary
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Datatypes for representing Harmonic Analyses (at the value level)
--------------------------------------------------------------------------------

-- H_armonic An_alysis wrapper datatype, the Int represents the duration
data HAn   = HAn      !Int !String 
           | HAnFunc  !HFunc
           | HAnPrep  !Prep
           | HAnTrans !Trans 
           | HAnChord !ChordToken
           deriving Generic
           
            -- duration Mode constructor_ix specials
data HFunc = Ton !Int !Mode !Int !(Maybe Spec)
           | Dom !Int !Mode !Int !(Maybe Spec) 
           | Sub !Int !Mode !Int !(Maybe Spec) 
           | P 
           | PD
           | PT
           deriving Generic
  
data Spec  = Blues | MinBorrow | Parallel          
  deriving (Eq, Generic)

-- Preparations, like secondary dominants etc. that cause a "split" in the tree  
data Prep  = SecDom   !Int !ScaleDegree -- "V/X"
           | SecMin   !Int !ScaleDegree -- "v/X"           
           | DiatDom  !Int !ScaleDegree -- "Vd"
           | NoPrep
           deriving Generic

-- Scalde degree transformations, e.g. tritone substitutions etc.
data Trans = Trit     !Int !ScaleDegree -- "bII/X"    
           | DimTrit  !Int !ScaleDegree -- "bIIb9/X"    
           | DimTrans !Int !ScaleDegree -- "VII0"      
           | NoTrans
           deriving Generic

--------------------------------------------------------------------------------
-- Binary instances
--------------------------------------------------------------------------------

instance Binary HAn
instance Binary Trans
instance Binary Prep
instance Binary HFunc
instance Binary Spec

--------------------------------------------------------------------------------
-- NFData instances
--------------------------------------------------------------------------------

instance NFData HAn where
  rnf (HAn d s   ) = rnf d `seq` rnf s
  rnf (HAnFunc  a) = rnf a
  rnf (HAnTrans a) = rnf a
  rnf (HAnPrep  a) = rnf a
  rnf (HAnChord a) = seq a ()

instance NFData HFunc where
  rnf (Ton a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d
  rnf (Dom a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d
  rnf (Sub a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d
  rnf P             = ()
  rnf PD            = ()
  rnf PT            = ()
  
instance NFData Prep where
  rnf (SecDom i d) = rnf i `seq` d `seq` ()
  rnf (SecMin i d) = rnf i `seq` d `seq` ()
  rnf (DiatDom i d) = rnf i `seq` d `seq` ()
  rnf NoPrep = ()
  
instance NFData Trans where  
  rnf (Trit   i d) = rnf i `seq` d `seq` ()
  rnf (DimTrit  i d) = rnf i `seq` d `seq` ()
  rnf (DimTrans i d) = rnf i `seq` d `seq` ()
  rnf NoTrans = ()
  
instance NFData Spec where
  rnf Blues     = ()
  rnf MinBorrow = ()
  rnf Parallel  = ()
  
--------------------------------------------------------------------------------
-- Durations set and get instances
--------------------------------------------------------------------------------  

-- Yes, I know these can be generic functions, but with my current generic 
-- programming skils it is faster two write them by hand.
class GetDur a where  
  getDur :: a -> Int
  
instance GetDur HAn where
  getDur (HAn  d  _s) = d
  getDur (HAnFunc  a) = getDur a  
  getDur (HAnPrep  a) = getDur a  
  getDur (HAnTrans a) = getDur a  
  getDur (HAnChord a) = dur a
  
instance GetDur HFunc where
  getDur (Ton i _ _ _) = i
  getDur (Dom i _ _ _) = i
  getDur (Sub i _ _ _) = i
  getDur _             = 0
  
instance GetDur Prep where
  getDur (SecDom i _) = i
  getDur (SecMin i _) = i
  getDur (DiatDom  i _) = i
  getDur NoPrep      = 0

instance GetDur Trans where  
  getDur (Trit   i _) = i
  getDur (DimTrit  i _) = i
  getDur (DimTrans i _) = i
  getDur NoTrans = 0

instance GetDur (Chord a) where
  getDur = duration
 
class SetDur a where  
  setDur :: a -> Int -> a
  
instance SetDur HAn where
  setDur (HAn  _ s)   i = (HAn  i s) 
  setDur (HAnFunc  a) i = (HAnFunc (setDur a i)) 
  setDur (HAnTrans a) i = (HAnTrans (setDur a i)) 
  setDur a           _i = a
  
instance SetDur HFunc where
  setDur (Ton _d m i s) d = (Ton d m i s)
  setDur (Dom _d m i s) d = (Dom d m i s)
  setDur (Sub _d m i s) d = (Sub d m i s)
  setDur a _  = a
  
instance SetDur Prep where
  setDur (SecDom   _d sd) d = (SecDom   d sd)
  setDur (SecMin   _d sd) d = (SecMin   d sd)
  setDur (DiatDom  _d sd) d = (DiatDom  d sd)
  setDur NoPrep _ = NoPrep
  
instance SetDur Trans where  
  setDur (Trit     _d sd) d = (Trit     d sd)
  setDur (DimTrit  _d sd) d = (DimTrit  d sd)
  setDur (DimTrans _d sd) d = (DimTrans d sd)
  setDur NoTrans _ = NoTrans
 
--------------------------------------------------------------------------------
-- Eq instances
--------------------------------------------------------------------------------  
 
instance Eq HAn where 
  (HAn _ s)        == (HAn _ s2)        = s     == s2
  (HAnChord chord) == (HAnChord chord2) = chord == chord2
  (HAnFunc  hfunk) == (HAnFunc  hfunk2) = hfunk == hfunk2
  (HAnTrans trans) == (HAnTrans trans2) = trans == trans2
  _ == _ = False
   
instance Eq HFunc where
  -- ignore duration for now
  (Ton _ b c d) == (Ton _ b2 c2 d2) = b == b2 && c == c2 && d == d2 
  (Dom _ b c d) == (Dom _ b2 c2 d2) = b == b2 && c == c2 && d == d2
  (Sub _ b c d) == (Sub _ b2 c2 d2) = b == b2 && c == c2 && d == d2
  P             == P   = True         
  PD            == PD  = True         
  PT            == PT  = True    
  _             == _   = False   
  
instance Eq Prep where
  (SecDom   _dur sd) == (SecDom   _dur2 sd2) = sd == sd2 
  (SecMin   _dur sd) == (SecMin   _dur2 sd2) = sd == sd2 
  (DiatDom  _dur sd) == (DiatDom  _dur2 sd2) = sd == sd2 
  NoPrep             == NoPrep = True
  _                  == _      = False
  
instance Eq Trans where  
  (Trit     _dur sd) == (Trit     _dur2 sd2) = sd == sd2 
  (DimTrit  _dur sd) == (DimTrit  _dur2 sd2) = sd == sd2 
  (DimTrans _dur sd) == (DimTrans _dur2 sd2) = sd == sd2 
  NoTrans           == NoTrans = True
  _                 == _       = False    

--------------------------------------------------------------------------------
-- Eq and Show instances
--------------------------------------------------------------------------------
  
instance Show Prep where
  show (SecDom   l d) = "V/"     ++ show d ++ '_' : show l
  show (SecMin   l d) = "v/"     ++ show d ++ '_' : show l
  show (DiatDom  l d) = "Vd/"++ show d ++ '_' : show l 
  show NoPrep = "np"

instance Show Trans where  
  show (Trit     l d) = "IIb/"   ++ show d ++ '_' : show l
  show (DimTrit  l d) = "IIb9b/" ++ show d ++ '_' : show l
  show (DimTrans l d) = show d   ++ "0"    ++ '_' : show l
  show (NoTrans)      = "nt"

instance Show HAn where 
  show (HAn l con)     = con ++ "_s"  ++ '_' : show l
  show (HAnChord chord) = show chord 
  show (HAnFunc  hfunk) = show hfunk 
  show (HAnTrans trans) = show trans 
  show (HAnPrep  prep ) = show prep
  
instance Show HFunc where
  show (Ton l mode i s) = "T" ++ show mode       ++ '_' : show i 
                              ++ maybe "" show s ++ '_' : show l
  show (Dom l mode i s) = "D" ++ show mode       ++ '_' : show i 
                              ++ maybe "" show s ++ '_' : show l
  show (Sub l mode i s) = "S" ++ show mode       ++ '_' : show i 
                              ++ maybe "" show s ++ '_' : show l
  show (P )             = "Piece"
  show (PT)             = "PT"
  show (PD)             = "PD" 

instance Show Spec where
  show Blues     = "bls" 
  show MinBorrow = "bor"
  show Parallel  = "par"
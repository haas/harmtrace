{-# OPTIONS_GHC -Wall #-}
module HarmTrace.Matching.Sim where

import HarmTrace.HAnTree.HAn
import HarmTrace.HAnTree.Tree
import HarmTrace.Base.MusicRep
import HarmTrace.Models.ChordTokens (ChordToken)

-------------------------------------------------------------------------------- 
-- A class for representing numerical similarity between datatypes
-------------------------------------------------------------------------------- 

class Sim a where
  sim :: a -> a -> Int
  
instance Sim a => Sim (Tree a) where
  sim (Node l _ _) (Node l' _ _) = sim l l'

instance Sim a => Sim [a] where
  sim [ha]    [hb]    = sim ha hb 
  sim (ha:ta) (hb:tb) = sim ha hb + sim ta tb
  sim _       _       = 0

instance Sim HAn where
  sim (HAn    _ a) (HAn    _ b) = if a == b then 1 else 0
  sim (HAnFunc  a) (HAnFunc  b) = sim a b
  sim (HAnPrep  a) (HAnPrep  b) = sim a b
  sim (HAnTrans a) (HAnTrans b) = sim a b
  sim (HAnChord a) (HAnChord b) = sim a b
  sim  _            _           = 0
  
instance Sim Int where
  {-# INLINE sim #-}
  sim i j = if i == j then 1 else 0

instance Sim HFunc where
  {-# INLINE sim #-}
  sim (Ton _ _m c _) (Ton _ _m2 c2 _) = if c == c2 then 1 else 0 -- 1 + sim m m2
  sim (Dom _ _m c _) (Dom _ _m2 c2 _) = if c == c2 then 1 else 0 -- 1 + sim m m2
  sim (Sub _ _m c _) (Sub _ _m2 c2 _) = if c == c2 then 1 else 0 -- 1 + sim m m2
  sim P             P              = 1      
  sim PD            PD             = 1      
  sim PT            PT             = 1 
  sim _             _              = 0
  
instance Sim Mode where
  {-# INLINE sim #-}
  sim MajMode MajMode = 1  
  sim MinMode MinMode = 1  
  sim _       _       = 0

instance Sim Trans where                                             
  {-# INLINE sim #-}
  sim (Trit     _v sd) (Trit     _v2 sd2) = if sd == sd2 then 1 else 0
  sim (DimTrit  _v sd) (DimTrit  _v2 sd2) = if sd == sd2 then 1 else 0
  sim (DimTrans _v sd) (DimTrans _v2 sd2) = if sd == sd2 then 1 else 0
  sim _ _ =0

instance Sim Prep where                                             
  {-# INLINE sim #-}  
  sim (DiatDom  _v sd) (DiatDom  _v2 sd2) = if sd == sd2 then 3 else 2
  sim (SecDom   _v sd) (SecDom   _v2 sd2) = if sd == sd2 then 3 else 2
  sim (SecMin   _v sd) (SecMin   _v2 sd2) = if sd == sd2 then 3 else 2
  
  sim (SecMin   _v sd) (DiatDom  _v2 sd2) = if sd == sd2 then 2 else 1
  sim (DiatDom  _v sd) (SecMin   _v2 sd2) = if sd == sd2 then 2 else 1
  
  sim (SecMin   _v sd) (SecDom   _v2 sd2) = if sd == sd2 then 2 else 1
  sim (SecDom   _v sd) (SecMin   _v2 sd2) = if sd == sd2 then 2 else 1
  
  sim (DiatDom  _v sd) (SecDom   _v2 sd2) = if sd == sd2 then 2 else 1
  sim (SecDom   _v sd) (DiatDom  _v2 sd2) = if sd == sd2 then 2 else 1
  -- sim  NoTrans           NoTrans = 
  sim _                 _        = 0      
  
instance Sim ChordToken where
  sim c1 c2 = if c1 == c2 then 2 else 0
  
-------------------------------------------------------------------------------- 
-- Some utility functions
--------------------------------------------------------------------------------  
  
-- calculates the self similarity value (used for normalisation) i.e. the
-- maximum similarity score
maxSim :: Sim a => [a] -> Int
maxSim =  foldr (\a b -> sim a a + b) 0  
   
div1 :: Int -> Int -> Int 
div1 n c = if n == 1 then 1 else n `div` c 
 
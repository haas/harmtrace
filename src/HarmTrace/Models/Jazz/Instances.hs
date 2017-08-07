{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE GADTs                  #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Models.Jazz.Instances
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Adhoc instances for the jazz model
--------------------------------------------------------------------------------

module HarmTrace.Models.Jazz.Instances where

-- Generics stuff
import Generics.Instant.TH

-- Parser stuff
import Text.ParserCombinators.UU hiding (P)
import Text.ParserCombinators.UU.BasicInstances

-- Music stuff
import HarmTrace.Models.Parser
import HarmTrace.Models.Jazz.Model
import HarmTrace.HAnTree.Tree
import HarmTrace.HAnTree.ToHAnTree
import HarmTrace.HAnTree.HAn
import HarmTrace.Models.ChordTokens as CT
import HarmTrace.Models.TypeLevel
import HarmTrace.Base.MusicRep

-- Library modules
import Control.Arrow

--------------------------------------------------------------------------------
-- The non-generic part of the parser
--------------------------------------------------------------------------------

instance ParseG (Base_SD   deg clss Ze) where parseG = empty

instance ( ToDegree (DiatV deg)
         , ToDegree (VDom deg)
         , ParseG (Base_SD (VDom   deg) DomClass n)
         , ParseG (Base_SD (DiatV  deg) MinClass n)
         , ParseG (Base_SD (DiatVM deg) MajClass n)
         , ParseG (Base_SD         deg  MinClass n)
         , ParseG (TritMinVSub     deg  MinClass  )
         ) => ParseG (Base_SD deg MinClass (Su n)) where
  parseG =     Base_SD   <$> parseG
           <|> Cons_Vdom <$> parseG <*> parseG
           <|> Cons_Diat <$> parseG <*> parseG
           <|> Cons_DiatM' <$> parseG <*> parseG

instance ( ToDegree (DiatVM deg)
         , ToDegree (VDom deg)
         , ParseG (Base_SD (VDom   deg) DomClass n)
         , ParseG (Base_SD (DiatVM deg) MajClass n)
         , ParseG (Base_SD         deg  MajClass n)
         , ParseG (TritMinVSub     deg  MajClass  )
         ) => ParseG (Base_SD deg MajClass (Su n)) where
  parseG =     Base_SD   <$> parseG
           <|> Cons_Vdom <$> parseG <*> parseG
           <|> Cons_DiatM <$> parseG <*> parseG

instance ( ToDegree (VMin deg)
         , ToDegree (VDom deg)
         , ParseG (Base_SD (VDom deg) DomClass n)
         , ParseG (Base_SD (VMin deg) MinClass n)
         , ParseG (Base_SD       deg  DomClass n)
         , ParseG (TritMinVSub   deg  DomClass  )
         ) => ParseG (Base_SD deg DomClass (Su n)) where
  parseG =     Base_SD   <$> parseG
           <|> Cons_Vdom <$> parseG <*> parseG
           <|> Cons_Vmin <$> parseG <*> parseG

instance ( ToDegree (VDom deg)
         , ParseG (Base_SD (VDom  deg) DomClass n)
         , ParseG (Base_SD        deg  DimClass n)
         , ParseG (TritMinVSub    deg  DimClass  )
         ) => ParseG (Base_SD deg DimClass (Su n)) where
  parseG =     Base_SD   <$> parseG
           <|> Cons_Vdom <$> parseG <*> parseG

-- Ad-hoc cases for Base_Final
instance ParseG (Base_Final deg clss Ze) where parseG = empty

instance ( ParseG (FinalDimTrans deg clss)
         ) => ParseG (Base_Final deg clss (Su n)) where
  parseG =     Base_Final  <$> parseG

instance ( ParseG (FinalDimTrans       deg  DomClass)
         , ParseG (FinalDimTrans       deg  MinClass)
         , ParseG (Base_Final (Tritone deg) DomClass n)
         , ParseG (Base_Final (IIbDim  deg) DimClass n)
         ) => ParseG (Base_Final deg DomClass (Su n)) where
  parseG =     Base_Final     <$> parseG
           <|> Final_Tritone  <$> parseG
           <|> Final_Dim_V    <$> parseG

-- Ad-hoc cases for Surface_Chord
instance ParseG (Surface_Chord deg clss Ze) where parseG = empty

instance ( ToDegree deg
         , ParseG (Surface_Chord (MinThird deg) DimClass n)
         ) => ParseG (Surface_Chord deg DimClass (Su n)) where
  parseG =     Dim_Chord_Trns <$> parseG
           <|> pChord deg DimClass
    where deg = toDegree (undefined :: deg)

-- all chords
instance ( ToDegree deg, ToClass clss
         ) => ParseG (Surface_Chord deg clss (Su n)) where
  parseG = pChord deg clss
    where deg = toDegree (undefined :: deg)
          clss = toClass (undefined :: clss)

-- generic ad-hoc parser that forms the bridge between the type-level and
-- value-level representation
pChord :: ScaleDegree -> ClassType -> PMusic (Surface_Chord deg clss (Su n))
-- Do not parse Imp degrees
pChord (Note _ Imp) _clss = empty
-- General case
pChord deg clss = setStatus <$> pSatisfy recognize insertion where
  {-# INLINE recognize #-}
  recognize ct = deg == root ct && clss == classType ct

  {-# INLINE setStatus #-}
  setStatus c = case status c of
    NotParsed -> Surface_Chord c {status = Parsed}
    _         -> Surface_Chord c

  insertion = Insertion "ChordToken" (ChordToken deg clss [] CT.Inserted 1 0) 5

--------------------------------------------------------------------------------
-- The non-generic part of the GTree wrapper
--------------------------------------------------------------------------------
toGTree :: (GetDegree a, GTree a) =>
           (Int -> ScaleDegree -> Trans) -> Int -> a -> [Tree HAn]
toGTree con transp deg = [Node (HAnTrans . con 1 $ toTransSDVal transp deg)
                               (gTree deg) Nothing]

-- create a branching Tree HAn
toGTreeSplit :: (GetDegree a, GetDegree b, GTree a, GTree b) =>
           (Int -> ScaleDegree -> Prep) -> b -> a -> [Tree HAn]
toGTreeSplit con vof deg
  = Node (HAnPrep . con 1 $ toSDVal deg) (gTree vof) Nothing : gTree deg

-- Ad-Hoc case for Piece
instance GTree Piece where -- we take the children to skip a "list node"
  gTree (Piece p) = [Node (HAnFunc P) (gTree p) Nothing]

-- Ad-hoc cases for Base_SD
instance GTree (Base_SD deg clss Ze) where
  gTree _ = error "gTree: impossible?"

instance ( GTree (Base_SD (VDom   deg)  DomClass n)
         , GTree (Base_SD (DiatV  deg)  MinClass n)
         , GTree (Base_SD (DiatVM deg)  MajClass n)
         , GTree (Base_SD (VMin   deg)  MinClass n)
         , GTree (Base_SD         deg   clss     n)
         , GTree (Base_Final      deg   clss     n)
         ) => GTree (Base_SD deg clss (Su n)) where
  gTree (Base_SD d)       = gTree d
  gTree (Cons_Vdom   s d) = toGTreeSplit SecDom  s d
  gTree (Cons_Diat   s d) = toGTreeSplit DiatDom s d
  gTree (Cons_DiatM  s d) = toGTreeSplit DiatDom s d
  gTree (Cons_DiatM' s d) = toGTreeSplit DiatDom s d
  gTree (Cons_Vmin   s d) = toGTreeSplit SecMin  s d

-- Ad-hoc cases for Base_Final
instance GTree (Base_Final deg clss Ze) where
  gTree _ = error "gTree: impossible?"

instance ( GetDegree (Base_Final (Tritone deg) DomClass n)
         , GetDegree (Base_Final (IIbDim  deg) DimClass n)
         , GTree (FinalDimTrans deg clss)
         , GTree (Base_Final (Tritone deg)  DomClass n)
         , GTree (Base_Final (IIbDim  deg)  DimClass n)
         ) => GTree (Base_Final deg clss (Su n)) where
  gTree (Base_Final d)      = gTree d
  -- The tritone substitution of a relative V is as alsway one semitone above
  -- the chord it is preceding
  gTree (Final_Tritone  d)  = toGTree Trit    6  d
  gTree (Final_Dim_V    d)  = toGTree DimTrit 11 d

-- Ad-hoc cases for Surface_Chord
instance GTree (Surface_Chord deg clss Ze) where
  gTree _ = error "gTree: impossible?"

instance ( GetDegree (Surface_Chord (MinThird deg) DimClass n)
         , GTree     (Surface_Chord (MinThird deg) DimClass n)
         ) => GTree  (Surface_Chord deg clss (Su n)) where
  gTree (Surface_Chord c)  = [Node (HAnChord c) [] Nothing]
  gTree (Dim_Chord_Trns c) = toGTree DimTrans 9 c -- pretty print?

--------------------------------------------------------------------------------
-- Ad hoc getDegree instaces
--------------------------------------------------------------------------------
toTransSDVal :: (GetDegree a) => Int -> a -> ScaleDegree
toTransSDVal t d = let (a,i) = getDeg d in transposeSem a (i+t)

toSDVal :: (GetDegree a) => a -> ScaleDegree
toSDVal d = let (a,i) = getDeg d in transposeSem a i

-- Given a degree getDegee ensures that all information about the internal
-- structure of a scale degree,i.e. the degree and the an int value representing
-- the transposition of that degree at the current level, is available.
class GetDegree a where
  getDeg :: a -> (ScaleDegree, Int)

instance GetDegree (Base_SD deg clss n) where
  getDeg (Base_SD d) = getDeg d
  getDeg (Cons_Vdom   _ d) = getDeg d
  getDeg (Cons_Diat   _ d) = getDeg d
  getDeg (Cons_DiatM  _ d) = getDeg d
  getDeg (Cons_DiatM' _ d) = getDeg d
  getDeg (Cons_Vmin   _ d) = getDeg d

instance ( GetDegree (Base_Final deg clss Ze)) where
  getDeg = error "getDegree: impossible?"
instance GetDegree (Base_Final deg clss  n) where
  getDeg (Base_Final d)  = getDeg d
  -- The tritone substitution of a relative V is as always one semitone above
  -- the chord it is preceding
  getDeg (Final_Tritone  d)  = second (+6) (getDeg d)
  getDeg (Final_Dim_V    d)  = second (+1) (getDeg d)

instance ( GetDegree (Surface_Chord deg clss Ze)) where
  getDeg = error "getDegree: impossible?"

instance ( GetDegree (Surface_Chord (MinThird deg) DimClass n)
         ) => GetDegree (Surface_Chord deg clss (Su n)) where
  getDeg (Surface_Chord (ChordToken d _cls _cs _stat _n _dur)) = (d,0)
  getDeg (Dim_Chord_Trns d) = second (+9) (getDeg d)

--------------------------------------------------------------------------------
-- Instances of Representable for music datatypes
--------------------------------------------------------------------------------

deriveAllL allTypes

$(fmap join $ mapM (\t -> gadtInstance ''ParseG t 'parseG 'parseGdefault)
  allTypes)

$(fmap join $ mapM (\t -> simplInstance ''GTree t 'gTree 'gTreeDefault)
  allTypes)

--------------------------------------------------------------------------------
-- ChordToken as tokens
--------------------------------------------------------------------------------

instance IsLocationUpdatedBy Int ChordToken where
  advance p c = p + chordNumReps c

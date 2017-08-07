{-# LANGUAGE CPP                      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE TypeSynonymInstances     #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE GADTs                    #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Models.Jazz.Model
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: The jazz model
--------------------------------------------------------------------------------

module HarmTrace.Models.Jazz.Model where

import HarmTrace.Models.TypeLevel

import HarmTrace.Base.MusicRep
import HarmTrace.Models.ChordTokens
import Language.Haskell.TH.Syntax (Name)

--------------------------------------------------------------------------------
-- Musical structure as a datatype
--------------------------------------------------------------------------------

#ifndef NUMLEVELS
#define NUMLEVELS T5
#endif

data MajMode
data MinMode

-- High level structure
data Piece = forall mode. Piece [Phrase mode]

-- The Phrase level
data Phrase mode where
  PT   :: Ton mode -> Phrase mode
  PD   :: Dom mode -> Phrase mode

-- Harmonic categories
-- Tonic
data Ton mode where
  -- major mode
  T_1      :: Final I MajClass         -> Ton MajMode
  T_2      :: Final I MajClass         -> Final IV MajClass 
           -> Final I MajClass         -> Ton MajMode
           
  -- blues           
  -- T_4_bls  :: Final I DomClass         -> Ton mode  

  T_3_par  :: Final III MinClass       -> Ton MajMode    
  T_6_bor  :: TMinBorrow -> Ton MajMode

  -- minor mode         
  Tm_1     :: SD MinMode I MinClass    -> Ton MinMode
  Tm_2     :: Final I MinClass -> Final IV MinClass
           -> Final I MinClass         -> Ton MinMode           

  Tm_3_par :: Final IIIb MajClass      -> Ton MinMode 
  Tm_6_bor :: TMajBorrow               -> Ton MinMode  -- picardy third etc.     

-- Dominant
data Dom mode where
  -- major mode
  D_1   :: SDom mode -> Dom mode -> Dom mode
  D_2   :: SD mode V DomClass        -> Dom mode
  D_3   :: SD mode V MajClass        -> Dom mode

  D_4   :: SD MajMode VII MinClass   -> Dom MajMode       
  
  -- moll-dur: minor mode borrowings in major
  -- This would be an elegant way of defining major minor borrowing,
  -- but it causes a lot of unwanted abmiguities since all the mode
  -- rules can be explained with and without borrowing
  -- D_9_bor :: Dom MinMode -> Dom MajMode
  D_8_bor :: DMinBorrow              -> Dom MajMode
  
  -- minor mode (there must be at least one rule with "MinMode" otherwise
  -- no you get a "No instance for (ParseG (Dom MinMode))" error
  Dm_4   :: SD MinMode VIIb MajClass -> Dom MinMode
  Dm_8_bor :: DMajBorrow             -> Dom MinMode

-- Subdominant
data SDom mode where
  S_1_par :: SD mode II MinClass     -> SDom mode -- sub dom parallel

  -- Pretty printing? this rule compensates for the V/I Imp
  -- to be able to parse D:7 D:min G:7 C:maj (not in cmj)
  S_2_par :: SD mode II DomClass -> Final II MinClass
                                         -> SDom mode 
  S_3     :: SD MajMode IV MajClass -> SDom MajMode
  S_4     :: SD MajMode III MinClass -> Final IV MajClass 
                                         -> SDom MajMode
  -- S_6_par :: SD MajMode VI MinClass  -> SDom MajMode
 
  -- blues
  -- S_2_bls :: SD mode IV DomClass  -> SD mode I DomClass 
                                         -- -> SDom mode 
  
  -- Borrowing from minor in a major mode
  -- S_7_bor :: SDom MajMode -> SDom MinMode
  S_5_bor :: SMinBorrow -> SDom MajMode 
  
  -- minor mode
  Sm_3    :: SD MinMode IV  MinClass  -> SDom MinMode
  Sm_4    :: SD MinMode IIIb MajClass -> Final IV MinClass 
                                          -> SDom MinMode
  -- Sm_3_par :: SD MinMode VIb MajClass -> SDom MinMode
 
  Sm_5_bor :: SMajBorrow              -> SDom MinMode 

  -- perhaps add a functional node for Neapolitan chords?
  Sm_6   :: SD MinMode IIb MajClass   -> SDom MinMode -- Neapolitan
  
-- Borrowings from minor in a major key
data TMinBorrow = Tm_21_bor (SD MinMode I    MinClass)   
                    | Tm_23_bor (SD MinMode IIIb MajClass)   

data DMinBorrow = Dm_24_bor (SD MinMode VIIb MajClass)   
                    --  Dm_21_bor (Final VIIb DomClass)   
                      
data SMinBorrow = Sm_20_bor (SD MinMode IV   MinClass) 
                    --  Sm_21_bor (SD MinMode VIb  MajClass)
                    | Sm_22_bor (SD MinMode IIb  MajClass)   -- Neapolitan 

-- Borrowings from major in a minor key
data TMajBorrow = T_21_bor (SD MajMode I   MajClass)
                    | T_23_bor (SD MajMode III MinClass)

data DMajBorrow = D_24_bor (SD MajMode VII MinClass)   
                    --  D_21_bor (Final VII DimClass)
                      
data SMajBorrow = S_20_bor (SD MajMode IV   MajClass) 
                    
                    
-- Limit secondary dominants to a few levels
type SD mode deg clss = Base_SD deg clss NUMLEVELS

-- a type that can be substituted by its tritone sub and diminished 7b9
type TritMinVSub deg clss = Base_Final deg clss T2

-- A Scale degree that can only translate to a surface chord
-- and allows for the transformation into enharmonic equivalent 
-- diminshed surface chords
type FinalDimTrans deg clss = Surface_Chord deg clss T4

-- A Scale degree that translates into a (non-tranformable) surface chord
type Final deg clss = Surface_Chord deg clss T1


-- Datatypes for clustering harmonic degrees
-- type Base_SD deg clss n = List (Base_SD' deg clss n) T4

data Base_SD deg clss n where
  Base_SD   :: TritMinVSub deg clss -- Min5    deg clss  n
            -> Base_SD deg clss (Su n)   
  -- Rule for explaining perfect secondary dominants
  Cons_Vdom :: Base_SD (VDom  deg) DomClass n -> Base_SD deg clss n
            -> Base_SD        deg  clss (Su n)
  Cons_Diat :: Base_SD (DiatV deg) MinClass n -> Base_SD deg MinClass n
            -> Base_SD        deg  MinClass (Su n)
  Cons_DiatM :: Base_SD (DiatVM deg) MajClass n -> Base_SD deg MajClass n
            -> Base_SD        deg  MajClass (Su n)
  Cons_DiatM' :: Base_SD (DiatVM deg) MajClass n -> Base_SD deg MinClass n
            -> Base_SD        deg  MinClass (Su n)
  -- Minor fifth insertion
  Cons_Vmin :: Base_SD (VMin deg) MinClass n -> Base_SD deg DomClass n
            -> Base_SD     deg  DomClass (Su n)

            
data Base_Final deg clss n where
  -- Just a "normal", final degree. The Strings are the original input.
  Base_Final     :: FinalDimTrans deg clss -> Base_Final deg clss (Su n)
  -- Tritone substitution
  Final_Tritone  :: Base_Final (Tritone deg) DomClass n
                 -> Base_Final deg DomClass (Su n)
  Final_Dim_V    :: Base_Final (IIbDim  deg) DimClass n
                 -> Base_Final deg DomClass (Su n)                 

-- Dimished tritone substitution accounting for dimished chord transistions
data Surface_Chord deg clss n where
  Surface_Chord  :: ChordToken  
                 -> Surface_Chord deg clss     (Su n)
  Dim_Chord_Trns :: Surface_Chord (MinThird deg) DimClass n
                 -> Surface_Chord deg DimClass (Su n)

--------------------------------------------------------------------------------
-- Type Level Scale Degrees
--------------------------------------------------------------------------------

-- typelevel chord classes 
data MajClass
data MinClass
data DomClass
data DimClass

-- Degrees (at the type level)
data I
data Ib
data Is
data II
data IIb
data IIs
data III
data IIIb
data IIIs
data IV
data IVb
data IVs
data V
data Vb
data Vs
data VI
data VIb
data VIs
data VII
data VIIb
data VIIs

-- Used when we don't want to consider certain possibilities
data Imp

-- Degrees at the value level are in Tokenizer
-- Type to value conversions
class ToClass clss where
  toClass :: clss -> ClassType

instance ToClass MajClass where toClass _ = MajClass
instance ToClass MinClass where toClass _ = MinClass
instance ToClass DomClass where toClass _ = DomClass
instance ToClass DimClass where toClass _ = DimClass

-- The class doesn't really matter, since the degree will be impossible to parse
instance ToClass Imp where toClass _ = DimClass

class ToDegree deg where
  toDegree :: deg -> ScaleDegree

instance ToDegree I     where toDegree _ = Note Nothing I
instance ToDegree II    where toDegree _ = Note Nothing II
instance ToDegree III   where toDegree _ = Note Nothing III
instance ToDegree IV    where toDegree _ = Note Nothing IV
instance ToDegree V     where toDegree _ = Note Nothing V
instance ToDegree VI    where toDegree _ = Note Nothing VI
instance ToDegree VII   where toDegree _ = Note Nothing VII
instance ToDegree Ib    where toDegree _ = Note (Just Fl) I
instance ToDegree IIb   where toDegree _ = Note (Just Fl) II
instance ToDegree IIIb  where toDegree _ = Note (Just Fl) III
instance ToDegree IVb   where toDegree _ = Note (Just Fl) IV
instance ToDegree Vb    where toDegree _ = Note (Just Fl) V
instance ToDegree VIb   where toDegree _ = Note (Just Fl) VI
instance ToDegree VIIb  where toDegree _ = Note (Just Fl) VII
instance ToDegree IIs   where toDegree _ = Note (Just Sh) II
instance ToDegree IIIs  where toDegree _ = Note (Just Sh) III
instance ToDegree IVs   where toDegree _ = Note (Just Sh) IV
instance ToDegree Vs    where toDegree _ = Note (Just Sh) V
instance ToDegree VIs   where toDegree _ = Note (Just Sh) VI
instance ToDegree VIIs  where toDegree _ = Note (Just Sh) VII

-- Can't ever parse these
instance ToDegree Imp where toDegree _ = Note Nothing Imp


--------------------------------------------------------------------------------
-- Type Families for Relative Scale Degrees
--------------------------------------------------------------------------------


-- Diatonic fifths, and their class (comments with the CMaj scale)
-- See http://en.wikipedia.org/wiki/Circle_progression
type family DiatV deg :: *
type instance DiatV I   = Imp -- V   -- G7  should be Dom
type instance DiatV V   = Imp -- II  -- Dm7 should be SDom
type instance DiatV II  = VI  -- Am7 
type instance DiatV VI  = III -- Em7
type instance DiatV III = VII -- Bhdim7 can be explained by Dim rule
type instance DiatV VII = Imp -- IV  -- FMaj7 should be SDom
type instance DiatV IV  = Imp -- I   -- CMaj7

type instance DiatV IIb  = Imp
type instance DiatV IIIb = Imp
type instance DiatV IVs  = Imp
type instance DiatV VIb  = Imp
type instance DiatV VIIb = Imp
type instance DiatV Imp  = Imp

type family DiatVM deg :: *
type instance DiatVM I   = Imp -- V   -- G7  should be Dom
type instance DiatVM V   = Imp -- Dm7 should be SDom
type instance DiatVM II  = VIb -- Ab 
type instance DiatVM VI  = Imp -- Em7
type instance DiatVM III = Imp -- Bhdim7 can be explained by Dim rule
type instance DiatVM VII = Imp -- IV  -- FMaj7 should be SDom
type instance DiatVM IV  = Imp -- I   -- CMaj7

type instance DiatVM IIb  = Imp
type instance DiatVM IIIb = VIIb 
type instance DiatVM IVs  = Imp
type instance DiatVM VIb  = IIIb
type instance DiatVM VIIb = Imp
type instance DiatVM Imp  = Imp

--------------------------------------------------------------------------------
-- Type families for secondary dominants
--------------------------------------------------------------------------------

-- Perfect fifths (class is always Dom)
-- See http://en.wikipedia.org/wiki/Circle_of_fifths
type family VDom deg :: *

type instance VDom I     = Imp  -- interferes with dom 
type instance VDom IIb   = VIb
type instance VDom II    = VI 
type instance VDom IIIb  = VIIb -- interferes with Dm_3
type instance VDom III   = VII
type instance VDom IV    = I
type instance VDom IVs   = IIb
type instance VDom V     = II   -- interferes with Sm_1
type instance VDom VIb   = IIIb
type instance VDom VI    = III
type instance VDom VIIb  = IV
type instance VDom VII   = IVs
type instance VDom Imp   = Imp

-- Perfect fifths for the minor case (this is an additional
-- type family to controll the reduction of ambiguities
-- specifically in the minor case)
type family VMin deg :: *
type instance VMin I     = V 
type instance VMin IIb   = VIb
type instance VMin II    = VI  -- interferes with sub 
type instance VMin IIIb  = VIIb
type instance VMin III   = VII
type instance VMin IV    = I
type instance VMin IVs   = IIb
type instance VMin V     = Imp -- II interferes with sub
type instance VMin VIb   = IIIb
type instance VMin VI    = III
type instance VMin VIIb  = Imp --IV -- inteferes with sub IV:min
type instance VMin VII   = IVs
type instance VMin Imp   = Imp
 
-- The tritone substitution
-- See http://en.wikipedia.org/wiki/Tritone_substitution
type family Tritone deg :: *
type instance Tritone I     = IVs
type instance Tritone IVs   = I

type instance Tritone IIb   = V    -- gives undesired (ambiguous results) as
type instance Tritone V     = IIb  -- Dom = IIb/I = IIbdim 

type instance Tritone II    = VIb -- interferes IIbDim V
type instance Tritone VIb   = II

type instance Tritone IIIb  = VI
type instance Tritone VI    = IIIb

type instance Tritone III   = VIIb -- Interferes with VIIb from minor
type instance Tritone VIIb  = III 

type instance Tritone IV    = VII
type instance Tritone VII   = IV

type instance Tritone Imp   = Imp  


--------------------------------------------------------------------------------
-- Type families for diminished chord transformations
--------------------------------------------------------------------------------
 
-- in combination with the secondary dominants and enharmonic equivalency
-- these type families account for ascending dim chord progressions
type family IIbDim deg :: *   
type instance IIbDim I     =  IIb
type instance IIbDim IIb   =  II
type instance IIbDim II    =  IIIb
type instance IIbDim IIIb  =  III
type instance IIbDim III   =  IV
type instance IIbDim IV    =  IVs
type instance IIbDim IVs   =  V
type instance IIbDim V     =  VIb -- interferes with dim tritone V/V
type instance IIbDim VIb   =  VI 
type instance IIbDim VI    =  VIIb
type instance IIbDim VIIb  =  VII
type instance IIbDim VII   =  I
type instance IIbDim Imp   =  Imp

-- Dimchords can be transposed a minor third without changing their role,
-- they are enharmonically equivalent.
type family MinThird deg :: *
type instance MinThird I     = IIIb 
type instance MinThird IIb   = III
type instance MinThird II    = IV
type instance MinThird IIIb  = IVs
type instance MinThird III   = V
type instance MinThird IV    = VIb
type instance MinThird IVs   = VI
type instance MinThird V     = VIIb 
type instance MinThird VIb   = VII
type instance MinThird VI    = I
type instance MinThird VIIb  = IIb
type instance MinThird VII   = II
type instance MinThird Imp   = Imp

-- Belongs in Instances, but needs to be here due to staging restrictions
allTypes :: [Name]
allTypes = [ ''Phrase, ''Ton, ''Dom, ''SDom
           , ''TMinBorrow, ''DMinBorrow, ''SMinBorrow 
           , ''TMajBorrow, ''DMajBorrow, ''SMajBorrow ]

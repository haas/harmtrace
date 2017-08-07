{-# LANGUAGE TemplateHaskell                #-}
{-# LANGUAGE EmptyDataDecls                 #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE DeriveGeneric                  #-}
{-# LANGUAGE GADTs                          #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans     #-}
module HarmTrace.Models.ChordTokens ( ChordToken (..)
                                    , PieceToken (..)
                                    , ParseStatus (..)
                                    , toKeyRelTok
                                    ) where

import HarmTrace.Base.MusicRep
import HarmTrace.Base.Instances ()
import Data.Binary
import GHC.Generics (Generic)
  
--------------------------------------------------------------------------------
-- Tokens for parsing chords
--------------------------------------------------------------------------------

-- merged Chords that will be presented to the parser
data ChordToken = ChordToken { root          :: ScaleDegree
                             , classType     :: ClassType
                             , chords        :: [ChordLabel]
                             , status        :: ParseStatus
                             , chordNumReps  :: Int
                             , dur           :: Int -- duration
                             } deriving Generic
                             
data ParseStatus = NotParsed | Parsed | Deleted | Inserted
  deriving (Eq, Show, Generic)
                             
-- a datatype to store a tokenized chords                              
data PieceToken = PieceToken Key [ChordToken]

-- (previously called mergeDups)
-- TODO rewrite function
-- | Merges duplicate chords and transforms absolute chord labels into key
-- relative tokens that can be parsed by the HarmTrace model 
toKeyRelTok  :: Key -> [ChordLabel] -> [ChordToken]
toKeyRelTok k (c@(Chord r _sh _add _loc d):cs) = toKeyRelTok' k 
      (ChordToken (toScaleDegree k r) (toClassType c) [c] NotParsed 1 d) cs
toKeyRelTok  _key [] = []
toKeyRelTok' :: Key ->  ChordToken -> [ChordLabel] -> [ChordToken]
toKeyRelTok' _k p [] = [p]
toKeyRelTok' k p@(ChordToken deg clss cs' _stat n d1) (c@(Chord r _sh _a _l d2):cs) 
  | deg == deg2 && clss == clss2 = 
      toKeyRelTok' k (ChordToken deg clss (cs' ++ [c]) NotParsed (n+1) (d1+d2)) cs
  | otherwise = p : toKeyRelTok' k (ChordToken deg2 clss2 [c] NotParsed 1 d2) cs
  where clss2 = toClassType c
        deg2  = toScaleDegree k r
        

--------------------------------------------------------------------------------
-- Instances for Chord Tokens
--------------------------------------------------------------------------------
instance Eq ChordToken where
  (ChordToken sd clss _cs stat _n _d) == (ChordToken sd2 clss2 _cs2 stat2 _n2 _d2) 
    = sd == sd2 && clss == clss2 && stat == stat2

instance Show ChordToken where
  show (ChordToken sd clss _cs Inserted _n _d) = show sd ++ show clss++"[Inserted]"
  show (ChordToken sd clss  cs Deleted  _n _d) = 
    show sd ++ show clss ++ "[Deleted" ++ showChords cs ++ "]"
  show (ChordToken sd clss  cs _ _n d) = show sd ++ show clss ++ '_' : show d 
                                                 ++ showChords cs    
showChords :: [ChordLabel] -> String  
showChords = concatMap (\x -> '[' : show x ++ "]") 


--------------------------------------------------------------------------------
-- Binary instances
--------------------------------------------------------------------------------

instance Binary ChordToken
instance Binary ParseStatus
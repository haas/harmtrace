{-# OPTIONS_GHC -Wall                     #-}
{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE TupleSections                #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Audio.Annotate
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Combining low-level features (VAMP plug-ins) with high-level
-- knowledge (the HarmTrace harmony model)
--------------------------------------------------------------------------------
module HarmTrace.Audio.Annotate ( mptreeAnnotator, groupAnnotator
                                , simpleAnnotator, mptreeAnnotatorSTG
                                , putSegStats, preProcessData
                                ) where
-- parameters
import Constants ( maxSegmentSize, maxLProductSize)
                 
-- Audio Stuff
import HarmTrace.Audio.ChromaChord ( createChordRanks, beatSync
                                   , mergeByOneAndThree )
import HarmTrace.Audio.Key (getBeatSyncKeyFromChroma)
import HarmTrace.Base.MusicTime

-- Harmony Model stuff
import HarmTrace.Base.MusicRep
import HarmTrace.Models.Models
import HarmTrace.Models.Jazz.Main
import HarmTrace.Models.Pop.Main
import HarmTrace.Models.ChordTokens
import HarmTrace.IO.Errors
import HarmTrace.HAnTree.HAn (HAn)
import HarmTrace.HAnTree.Tree (Tree, size, depth)
import HarmTrace.HAnTree.ToHAnTree (GTree)
import HarmTrace.HarmTrace

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances

import System.IO (stdout,hFlush)
import Data.List (sortBy, groupBy, intersperse)
import Control.Arrow (first)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- From chords with probabilities to a single chord, using harmony
--------------------------------------------------------------------------------

mptreeAnnotatorSTG :: GrammarEx -> Maybe [TimedData Key] -> AudioFeat 
                 -> ChordAnnotation
mptreeAnnotatorSTG gex k = snapToGrid . mptreeAnnotator gex k
                 
-- | MPTrEE (Model Propelled Transcription of Euphonic Enitities): 
-- a sophisticated, harmony and beat informed chord annotator
mptreeAnnotator :: GrammarEx -> Maybe [TimedData Key] -> AudioFeat 
                 -> ChordAnnotation
mptreeAnnotator (GrammarEx g) k f = concatMap (harmonize g) (preProcessData k f)

-- | preprocesses the raw audio data before using chord harmony model based
-- chord selection. First, the beats and chroma are synchronised. Second, 
-- chord candidate lists are created. Third, smart, beat informed grouping of 
-- the chord candidates is performed. Fourth, the chord candidate lists are 
-- grouped in segments based on the key (obtained
-- as provided by the user or as derived from the audio data). Last, the 
-- chord candidate lists are further segmented based on the occurrences of 
-- I and V chords.
preProcessData ::  Maybe [TimedData Key] -> AudioFeat -> [ProbChordSeg]
preProcessData gtk af@(AudioFeat chrm beats _afk _id) =
  segmentByTonic $ segmentByKey key . mergeByOneAndThree 
                                    . createChordRanks $ beatSync beats chrm  
    where key = maybe (getBeatSyncKeyFromChroma af) id gtk

-- reminder: ProbChordSeg = Segment Key [TimedData [ProbChord]] 
-- | Chord selection based on the harmtrace model 
harmonize :: forall g. (GTree g) => Grammar g -> ProbChordSeg 
          -> ChordAnnotation
harmonize g (Segment k cands) =
  let isExpandable :: Bool
      isExpandable = length (filter ((>1) . length) (map getData cands)) > 0
      
      myParse :: [ProbChord] -> (Tree HAn,[ProbChord],Float)
      myParse cs =
        let x = map probChord cs
        
            -- First, parse the tokens
            res :: ([g],[Error Int])
            res = case g of 
                      Jazz -> parse_h ((,) <$> pJazz k <*> pEnd) (createStr 0 x)
                      Pop  -> parse_h ((,) <$> pPop  k <*> pEnd) (createStr 0 x)
            -- Build a ParseResult from that
            pr = ParseResult u (concatMap chords x) (fst res) u u u (snd res) []
            -- So that we can post-process it. Then extract the Tree HAn
            t  = pieceTreeHAn (postProc [ RemovePDPT, MergeDelChords ]   pr)
            u :: forall a. a
            u  = error "harmonize: undefined placeholder evaluated"
        -- Return the Tree HAn, the input tokens, and the error-ratio
        in (t, cs, errorRatio (snd res) x )

      -- Generate, parse, and evaluate all possible sequences of chords
      parseResults :: [(Tree HAn,[ProbChord],Float)]
      parseResults = [ myParse l
                     | l <- lProduct (map getData cands) ]

      -- From all possible parse trees, take the best one
      select :: [(Tree HAn,[ProbChord],Float)] -> [ProbChord]
      select = select1 . head
             . groupBy (\(_,_,a) (_,_,b) -> a `compare` b == EQ)
             . sortBy  (\(_,_,a) (_,_,b) -> a `compare` b)

      -- These all have the same error ratio, so we sort them first by tree
      -- size, then depth, and pick the first
      select1 :: [(Tree HAn,[ProbChord],Float)] -> [ProbChord]
      select1 = snd3 . head . sortBy cmp where
        cmp (a,_,_) (b,_,_) = (size a, depth a) `compare` (size b, depth b)
        snd3 (_,s,_) = s

      probChord :: ProbChord -> ChordToken
      probChord (ProbChord lab@(Chord r _sh _add _on _dur) _p) = 
        (ChordToken r' sh' [lab] NotParsed 1 0) where
           r'  = if isNone r   then Note Nothing Imp else toScaleDegree k r
           sh' = toClassType lab
      
      -- replaces the candidate list by the selected chord in a TimedData
      setBestChords :: [ProbChord] -> [TimedData ProbChord]
      setBestChords = zipWith setData cands 

      -- if there is nothing to expand, do not parse
  in if   isExpandable then setBestChords $ select parseResults 
     else map pickHead cands

pickHead :: TimedData [ProbChord] -> TimedData ProbChord
pickHead = fmap head

--------------------------------------------------------------------------------
-- post-processing functions
--------------------------------------------------------------------------------

-- snapToGrid :: ChordAnnotation -> ChordAnnotation -- = [TimedData ProbChord]
snapToGrid :: [TimedData ProbChord] -> [TimedData ProbChord] 
snapToGrid = foldr snap [] . reduceTimedPChords where
  
  snap :: TimedData a -> [TimedData a] -> [TimedData a]
  snap td []       = [td]
  snap a  (h : tl)  = case ( odd (beatLen a) && odd (beatLen h) , getBeat h ) of
                        (True, Two ) -> shiftFwd a h ++ tl
                        (True, Four) -> shiftBwd a h ++ tl
                        _            -> a : h : tl

  beatLen :: TimedData a -> Int
  beatLen = pred . length . getTimeStamps  
  
-- Shifts the second TimedData backwards in time, lengthing the first TimedData
shiftBwd :: TimedData a -> TimedData a -> [TimedData a]
shiftBwd (TimedData a ta) tdb = case getTimeStamps tdb of
  [_on, off]       -> [TimedData a (ta ++ [off])                        ]
  (_hb : hhb : tb) -> [TimedData a (ta ++ [hhb]), tdb{getTimeStamps = (hhb:tb)}]
  [_] -> error "HarmTrace.Audio.Annotate.shiftBwd: 1 timestamp, onset == offset"
  [ ] -> error "HarmTrace.Audio.Annotate.shiftBwd: No timestamps to shift"

-- Shifts the second TimedData forwards in time at the cost of the first
shiftFwd :: TimedData a -> TimedData a -> [TimedData a]
shiftFwd tda (TimedData b tb) = case getTimeStamps tda of
  [ ] -> error "HarmTrace.Audio.Annotate.shiftFwd: No timestamps to shift"
  [_] -> error "HarmTrace.Audio.Annotate.shiftFwd: 1 timestamp, onset == offset"
  [on, _off] -> [TimedData b (on : tb)                               ]
  ta         -> [tda {getTimeStamps = initTa}, TimedData b (oneButLastTa : tb)] 
     where 
      (initTa,oneButLastTa) = snocsnoc ta

      -- takes a list of elements and returns, all elements up to the one-but-
      -- last element (discarding the last elem), and the one-but last element
      snocsnoc :: [a] -> ([a],a)
      snocsnoc []       = error "snocsnoc: empty list"
      snocsnoc [_]      = error "snocsnoc: singleton list"
      snocsnoc [x,_lst] = ([x], x) -- = the one-but-last element
      snocsnoc (x:xs)   = first (x :) (snocsnoc xs)

-- | Returns the reduced chord sequences, where repeated chords are merged
-- into one 'ProbChord', wrapped in a 'TimedData' type.
reduceTimedPChords :: [TimedData ProbChord] -> [TimedData ProbChord]
reduceTimedPChords = foldr group [] where

   group :: TimedData ProbChord -> [TimedData ProbChord] -> [TimedData ProbChord]
   group c [] = [c]
   group tc@(TimedData c tsc ) (th@(TimedData h tsh ) : t)
     | c `pChordEq` h = concatTimedData c {prob = avgProb} tc th : t
     | otherwise       = tc : th : t where
          
         avgProb :: NumData
         avgProb = let ltsc = fromIntegral $ length tsc
                       ltsh = fromIntegral $ length tsh
                       tot  = ltsc + ltsh
                   in (prob c * ltsc) + (prob h * ltsh) / tot
   
   pChordEq :: ProbChord -> ProbChord -> Bool
   pChordEq (ProbChord cA _pA) (ProbChord cB _pB) =
     chordRoot cA      == chordRoot cB && 
     chordShorthand cA == chordShorthand cB   
  
--------------------------------------------------------------------------------
-- Segmentation functions
--------------------------------------------------------------------------------
-- Temporary test values
{-
test = segmentTonic testKey testSeq
testKey = Key (Note Nothing C) MajMode
testSeq = testChordG ++ testChordC ++ testChordC ++ testChordG ++ testChordG
testChordC = [TimedData [ProbChord labC 1, ProbChord labG 0.5] 0 0]
testChordG = [TimedData [ProbChord labG 1, ProbChord labC 0.5] 0 0]
labC = Chord (Note Nothing C) Maj [] 0 0
labG = Chord (Note Nothing G) Maj [] 0 0
-}


-- move to segmentations function in Harmonize?
segmentByKey :: [TimedData Key] -> [TimedData [ProbChord]] -> [ProbChordSeg]
segmentByKey []       _    = error "segmentByKey: empty key list"
segmentByKey [k]      chds = [Segment (getData k) chds]
segmentByKey (k : ks) chds = let (seg,cs) = span ((<= offset k) . offset) chds
  in Segment (getData k) seg : segmentByKey ks cs

segmentByTonic :: [ProbChordSeg] -> [ProbChordSeg]
segmentByTonic segs = concatMap emergencySplit $ concatMap split segs where
  split :: ProbChordSeg -> [ProbChordSeg]
  split (Segment key cs) = zipWith Segment (repeat key) (segmentTonic key cs)

-- In case segments are just to big, even after segmenting on Tonic and Dominant
-- split these segments into smaller segements recursively.
emergencySplit :: ProbChordSeg -> [ProbChordSeg]
emergencySplit (Segment k cs) = map (Segment k) (recSplit cs) where
  -- recSplit :: [TimedData [a]] -> [[TimedData [a]]]
  recSplit [] = []
  recSplit b
    |    blen               <= maxSegmentSize 
      && snd (lProdStats b) <= maxLProductSize = [b]
    | otherwise   = recSplit l ++ recSplit r
      where blen  = length b
            (l,r) = splitAt (blen `div` 2) b 
  
-- Break into segments according to the key
segmentTonic :: Key -> [TimedData [ProbChord]] -> [[TimedData [ProbChord]]]
segmentTonic k cands = segment cands [] where 
  segment []     []     = []
  segment []     acc    = [reverse acc]
  segment (c:cs) acc
    | c' `isTonic` k || c' `isDom` k = reverse (c:acc) : segmentTonic k cs
    | otherwise                      = segment cs (c:acc) where 
        c' = getFstChord c

-- Take the first chord (which is the one with the highest probability, since
-- the list is sorted)
getFstChord :: TimedData [ProbChord] -> ChordLabel
getFstChord c = case getData c of
                  []    -> error "getFstChord: empty list"
                  (h:_) -> chordLab h -- only split on chords we are certain of
                -- _   -> Chord (Note Nothing N) None [] 0 0 -- else return None

-- Check if this chord label is the tonic
isTonic :: ChordLabel -> Key -> Bool
isTonic  (Chord (Note Nothing N) _ _ _ _) _ = False
isTonic c (Key r m) = r == chordRoot c && m == toMode (toTriad c)

-- Check if this chord label is the dominant
-- JPM: I don't understand why this function looks so different from `isTonic`
isDom :: ChordLabel -> Key -> Bool
isDom (Chord (Note Nothing N) _ _ _ _) _ = False
isDom c key       =    toScaleDegree key (chordRoot c) == Note Nothing V
                    && toTriad c == MajTriad


lProduct :: [[a]] -> [[a]]
lProduct []    = []
lProduct [l]   = [ [x] | x <- l ]
lProduct (h:t) = concat [ map (x:) (lProduct t) | x <- h ]

--------------------------------------------------------------------------------
-- Some printing and statistics functions
--------------------------------------------------------------------------------

-- | prints Segmetation statistics
putSegStats :: Maybe [TimedData Key] -> AudioFeat -> IO()
putSegStats k af = mapM_ segmentStat $ preProcessData k af 

segmentStat :: ProbChordSeg -> IO ()
segmentStat (Segment k bs) = 
  do putStr ("\nstart: "  ++ (printf "%.3f" . onset  $ head bs))
     putStr (", end: "  ++ (printf "%.3f" . offset $ last bs))
     putStr (", key: "  ++ show k)
     putStr (", probChords: " ++ show (length bs))
     let (l, lpr) = lProdStats bs
     putStr (", lists > 1: " ++ show l)
     putStrLn (" lProduct: " ++ show lpr)
     (putStrLn . concat . intersperse "\n" . map showTimedData $ bs)  
        >> hFlush stdout where

          showTimedData :: TimedData [ProbChord] -> String
          showTimedData td =  
            (concat . intersperse ", " . map showProbChord . getData $ td) 
                                   ++ ": " ++ ( show . getTimeStamps $ td )

          showProbChord :: ProbChord -> String
          showProbChord (ProbChord lab p) = show lab ++ '@' : printf "%.3f" p
     
-- Given a Block list this function returns the number of probChords with a 
-- list > 1 (fst) and the lProduct size (snd)
lProdStats :: [TimedData [a]] -> (Int, Int)
lProdStats bs = (length l, lpr) where
  l   = filter ((>1) . length ) (map getData bs)
  lpr = foldr (\a b -> length a * b) 1 l

--------------------------------------------------------------------------------
-- A baseline chord label annotator
--------------------------------------------------------------------------------

-- | Creates an annotation out of a Chord candidate list by just picking the 
-- first chord. This annotator does smart grouping 
-- (see 'HarmTrace.Audio.ChromaChord.mergeByBeat').
groupAnnotator :: GrammarEx -> Maybe [TimedData Key] -> AudioFeat -> ChordAnnotation
groupAnnotator _g _keyAnn (AudioFeat chrm beats _key _id) = -- ignore key info
  -- TODO: check is this synchronisation still needed???
  let endTime = BarTime (time $ last chrm) Four
      beats'  = takeWhile (< endTime) beats ++ [endTime]
  in map pickHead . mergeByOneAndThree  
                  . createChordRanks   $ beatSync beats' chrm  


-- | The most simple annotator, no grouping, no matching, 
-- just pick the best matching chord
simpleAnnotator :: GrammarEx -> Maybe [TimedData Key] -> AudioFeat -> ChordAnnotation
simpleAnnotator _g _keyAnn (AudioFeat crm bts _key _id) = -- ignore key
  map pickHead . createChordRanks $ beatSync bts crm

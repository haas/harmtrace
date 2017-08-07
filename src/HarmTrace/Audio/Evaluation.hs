--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Audio.Evaluation
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: A module for evaluating chord and key annotations
--------------------------------------------------------------------------------


module HarmTrace.Audio.Evaluation (
    -- * Evaluation functions
      relCorrectOverlap
    , achievScore
    , chordChangeRatio
    , avgDistToOne
    -- * Chord and key equality functions
    , chordTriadEq
    , chordClassEq
    , majMinEq
    -- * Displaying evaluations 
    , printChordRCO
    , printRCO
    -- * Sampling 
    , sample
  ) where

import Constants
import HarmTrace.Base.MusicTime
import HarmTrace.Audio.Annotate (preProcessData)
import HarmTrace.Base.MusicRep 

import Data.List (genericLength, zipWith5, foldl')
import Text.Printf(printf)
import System.IO (stdout,hFlush)
import Data.Foldable (foldrM)
import Control.Monad.State (State, execState, modify)

-- TODO this is a parameter and should some how be integrated into Constants.hs
-- this functions determines when two chords are considered the same
eqFunc :: ChordLabel -> ChordLabel -> Bool
eqFunc = chordTriadEq

--------------------------------------------------------------------------------
-- Chord and key equality functions
--------------------------------------------------------------------------------

-- | Returns True if both 'ChordLabel's are equal at the chord class level: 
-- A chord is classified as being major, minor, dominant seventh, or dimished
-- seventh. 'chordClassEq' only returns True if the class of compared chords
-- is the same. "None Chords" match only with other None Chords and 
-- with nothing else
chordClassEq :: ChordLabel -> ChordLabel -> Bool
chordClassEq a b =  chordRoot   a `rootEQ` chordRoot   b
                 && toClassType a  ==      toClassType b   

-- | Returns True if both 'ChordLabel's are equal at the triad level: they are
-- either moth major or both minor. "None Chords" match only with other "None
-- Chords" and with nothing else
chordTriadEq :: ChordLabel -> ChordLabel -> Bool
chordTriadEq a b =  chordRoot         a  `rootEQ` chordRoot         b
                 && toMajMin (toTriad a)  ==      toMajMin (toTriad b)

-- | Returns True if the 'Root's of the 'Chord's are equal, but the one chord
-- is Major and the other chord is Minor.
majMinEq :: ChordLabel -> ChordLabel -> Bool
majMinEq a b =  chordRoot a `rootEQ`  chordRoot b
             && toTriad   a `triadEq` toTriad   b where
   
   -- ingore the NoClass and only return True in case of maj/min and min/maj
   triadEq :: Triad -> Triad -> Bool
   triadEq x y = case (toMajMin x, toMajMin y) of
                   (MajClass, MinClass) -> True
                   (MinClass, MajClass) -> True
                   _                    -> False
             
             

-- | enharmonic equality for 'Root' 'Note's, N == N, X == X, and G# == Ab
rootEQ :: Root -> Root -> Bool
rootEQ (Note Nothing X) (Note Nothing X) = True  -- two unknown roots
rootEQ (Note Nothing N) (Note Nothing N) = True  -- two none roots
rootEQ (Note Nothing X) _                = False -- one unknown root
rootEQ _               (Note Nothing X)  = False
rootEQ (Note Nothing N) _                = False -- one none root
rootEQ _               (Note Nothing N)  = False
rootEQ a               b                 = toSemitone a == toSemitone b

--------------------------------------------------------------------------------
-- Evaluation functions
--------------------------------------------------------------------------------  
  
-- | Calculates the relative correct overlap, which is the recall
-- of matching frames, and defined as the nr of matching frames (sampled at
-- an 10 milisecond interval) divided by all frames.
relCorrectOverlap :: (a -> a -> Bool) -> [TimedData a] -> [TimedData a] 
                  -> Double
relCorrectOverlap eq a b = foldl' countMatch 0 (zipWith eq sama samb) / tot 
  where sama = sample a
        samb = sample b
        tot  = max (genericLength sama) (genericLength samb)

countMatch :: Double -> Bool -> Double 
countMatch x y | y         = succ x -- count the number of matching frames
               | otherwise = x

-- | Given a chord annotation sample the chord label at every 10 ms
sample :: [TimedData a]-> [a]
sample = sampleWith evaluationSampleRate

-- like sample, but takes a sample rate (seconds :: Float) as argument
sampleWith :: NumData -> [TimedData a] -> [a]
sampleWith rate =  sampleAt [0.00, rate .. ] 

        
-- samples at specific points in time, specified in a list
sampleAt :: [NumData] -> [TimedData a] -> [a]
sampleAt  _  [] = [] -- below, will never occur
sampleAt []  _  = error "Harmtrace.Audio.Evaluation: No sampling grid specified" 
sampleAt (t:ts) (c:cs)
  | t <= offset c = getData c : sampleAt ts (c:cs)
  | otherwise     = sampleAt (t:ts) cs         

-- | calculates the maximal achievable score given a ground truth annotation
-- and a chord candidate list.
achievScore :: [TimedData ChordLabel] -> [TimedData [ChordLabel]] -> Double
achievScore a b = sum (zipWith eq sama samb) / len
  where sama = sample a
        samb = sample b
        len  = min (genericLength sama) (genericLength samb)
        eq c cs | foldr (\x -> (chordTriadEq c x ||)) False cs = 1.0
                | otherwise                                    = 0.0  

-- | calculates the number of chord changes in the ground-truth divided 
-- by the number of chord changes in the machine annotation. A number < 1 
-- indicates that the machine annotation misses some chord changes. A number
-- > 1 indicates that the machine annotation finds to many chord sequences.
chordChangeRatio ::  (ChordLabel -> ChordLabel -> Bool) 
                 -> [TimedData ChordLabel] -> [TimedData ChordLabel] -> Double
chordChangeRatio eq gt ma = (fromIntegral . countChordChanges $ gt)
                          / (fromIntegral . countChordChanges $ ma) where

  countChordChanges :: [TimedData ChordLabel] -> Int
  countChordChanges cs = execState (foldrM step [] $ dropTimed cs) 0 

  step :: ChordLabel -> [ChordLabel] -> State Int [ChordLabel]
  step c []     = do modify succ
                     return [c]
  step a ( b : cs ) 
    | a `eq` b  =    return (a : b : cs)
    | otherwise = do modify succ
                     return (a : b : cs)

-- | The 'chordChangeRatio' is optimal if it is one, but it can be larger or 
-- smaller than 1. Therefore, calculating the average blurs the actual result.
-- 'avgDistToOne' takes the absolute difference to 1.0 and averages these for a
-- list of Doubles.
avgDistToOne :: [Double] -> Double
avgDistToOne ds = (sum . map absDistToOne $ ds) / genericLength ds where

  absDistToOne :: Double -> Double
  absDistToOne a = abs (1.0 - a)

--------------------------------------------------------------------------------
-- Displaying evaluations (all in IO)
--------------------------------------------------------------------------------    
  
-- | does the same thing as relCorrectOverlap, but it also prints the
-- chords and uses a lower sample rate. N.B. the number output by 
-- 'printRelCorrectOverlap' might differ from the output of 
-- 'relCorrectOverlap', because a different sample rate might be used (see
-- 'Constants').
printChordRCO :: (AudioFeat -> ChordAnnotation) -> [TimedData Key] 
              -> AudioFeat  -> [TimedData ChordLabel] -> IO Double
printChordRCO annotator key af gt = do
  let -- BUG:  now alswo when we are evaluating a simple annotator grouping is 
      --       is displayed, this is wrong. printRelCorrectOverlap should
      --       be independend of the kind of annotator.
      blks :: [TimedData [ProbChord]]
      blks  = concatMap segChords $ preProcessData Nothing af

      -- sample the info for printing and evaluation
      samaf = sampleWith displaySampleRate (dropProb . annotator $ af)
      samgt = sampleWith displaySampleRate gt
      sambk = sampleWith displaySampleRate blks
      samk  = sampleWith displaySampleRate key

      tot   = max (genericLength samaf) (genericLength samgt)
      showEq m = if m then "==" else "/=" 
      printEval :: NumData -> ChordLabel -> ChordLabel -> Key -> [ProbChord] 
                -> IO Bool
      printEval t g a b c = 
         do putStrLn (printf "%.2f" t ++ '\t' : showEq equal ++ '\t' : show g
                      ++ '\t' : show a ++ '\t' : show b ++ '\t' : show c)
                      >> hFlush stdout
            return equal where equal = g `eqFunc` a
  putStrLn "time\tmatch\tGT\t\tMPTREE\tkey\toptional chords"
  m <- sequence (zipWith5 printEval [0.0,displaySampleRate ..] 
                                    samgt 
                                    samaf 
                                    samk 
                                    sambk)
  return (foldl countMatch 0 m / tot)
  
  
-- | Calculates the relative correct overlap, which is the recall
-- of matching frames, and defined as the nr of matching frames (sampled at
-- an interval set in 'HarnTrace.Constants' divided by all frames.
-- This functions difers from 'relCorrectOverlap' in that it uses an
-- equality function that is in IO.
printRCO :: (a -> a -> IO (Bool)) -> [TimedData a] -> [TimedData a]
         -> IO (Double)
printRCO ioeq a b = do matches <- sequence (zipWith3 printEq [0,displaySampleRate ..] sama samb)
                       return (foldl' countMatch 0 matches / tot)
  where sama = sampleWith displaySampleRate a
        samb = sampleWith displaySampleRate b
        tot  = max (genericLength sama) (genericLength samb)  

        -- printEq :: NumData -> a -> a -> IO (Bool)
        printEq ts x y = do putStr (printf "%.2f: " ts)
                            ioeq x y

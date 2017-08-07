{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall         #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Audio.ChromaChord
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Recognise audio chroma vectors into textual chord descriptions.
--------------------------------------------------------------------------------

module HarmTrace.Audio.ChromaChord ( createChordRanks
                                    , beatSync
                                   -- , mergeByBeat
                                   , meanBeatSyncVectors
                                   , mergeByOneAndThree
                                   ) where

import Constants (maxProbChordListLength, cutOffProbability)

import HarmTrace.Audio.DataParser (shift)
import HarmTrace.Base.MusicTime
import HarmTrace.Base.MusicRep

-- import Text.Printf (printf)
import Data.List (sortBy,find) -- , elemIndices, minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

import HarmTrace.Audio.VectorNumerics
import Data.Vector                 ( Vector, slice, generate )
import Data.Matrix                 ( Matrix, fromLists, ncols, nrows, getCol, prettyMatrix  )

--------------------------------------------------------------------------------
-- Matching chords and chroma
--------------------------------------------------------------------------------

-- | Synchronises the 'ChordinoData' with the list of beats
-- by grouping the 'ChordinoLines' of the 'ChordinoData' in separate lists.
beatSync :: BarTimeTrackData -> [ChordinoLine] -> [BeatChroma] -- = [TimedData ChordinoLine]
beatSync _   [] = []
beatSync []  _  = error "HarmTrace.Audio.ChromaChord: no beat tracker data"
beatSync bt cs = beatTime fstBeat syncbt beatAlignedChroma where

  fstBeat  = BarTime 0                  (prevBeat . beat . head $ bt)
  lstBeat  = BarTime (time . last $ cs) (nextBeat . beat . last $ bt)
  syncbt   = takeWhile (< lstBeat) bt ++ [lstBeat]
  beatAlignedChroma = groupChroma (getBeatTrack syncbt) [head cs] cs

  -- groups a list of 'ChromaLine's into beat synchronised group:
  -- one list per beat
  groupChroma :: [NumData] -> [ChordinoLine] -> [ChordinoLine] -> [[ChordinoLine]]
  groupChroma _  _   [] = []
  groupChroma [] _   c  = [c] -- TODO: prv should be first  arg
  groupChroma (b:bs) prv c -- we also store the previous group in case beat < time
    | null x    = prv : groupChroma bs prv c -- Why do we need this?
    | otherwise = x   : groupChroma bs x   xs
        where (x, xs) = span ((b >=) . time) c

  -- given beatTracker data and a grouped chroma, the grouped chroma is wrapped
  -- into a TimedData type
  beatTime :: BarTime ->  [BarTime] -> [a] -> [TimedData a]
  -- TODO replace last by this case
  -- beatTime (BarTime (on, onbt)) [] [c]  = TimedData c onbt on (time . last $ c)
  beatTime _  [] [] = []
  beatTime (BarTime on onbt) (next@(BarTime off _) : bs) (x : xs) =
    TimedData x [BarTime on onbt, BarTime off (nextBeat onbt)] : beatTime next bs xs
  beatTime _  _  _  = error "beatSync: asynchronous beats and data"


-- | Merges chord segments, adding a bias toward merging at the first
-- and the third 'Beat'
mergeByOneAndThree :: [TimedData [ProbChord]] -> [TimedData [ProbChord]]
mergeByOneAndThree = mergeByBeat canMerge intersectPC where
  -- specifies which combinations of beats are allowed to merge
  canMerge :: Beat -> Beat -> Bool
  canMerge One   _ = True
  canMerge Three _ = True
  canMerge _     _ = False

-- Conditionally combines lists wrapped in a 'BeatTimeData' in a list. The first
-- argument should determine at which combinations of beats the merging function
-- (the second argument) should be applied. N.B. this function cannot
-- be rewritten with a foldr, because the outcome is dependend of the
-- left-to-right processing. Rewriting the function with foldl makes the
-- whole recognition process about 1/3 slower...
mergeByBeat :: (Beat -> Beat -> Bool) -> ([a] -> [a] -> [a])
            -> [TimedData [a]] -> [TimedData [a]]
mergeByBeat _ _ [] = []
mergeByBeat _ _ [a] = [a]
mergeByBeat canMerge merge (x:y:xs)
  | canMerge (getBeat x) (getBeat y)
    && not (null m) =     mergeByBeat canMerge merge (xy:xs)
  | otherwise       = x : mergeByBeat canMerge merge ( y:xs)
      where xy = TimedData m (getTimeStamps x ++ (tail . getTimeStamps $ y))
            m  = merge (getData x) (getData y)

-- | Calculates the intersection of to ['ProbChord]' N.B. because of the
-- set-based nature of the function the function is rather expensive, luckily
-- the length of the ['ProbChord'] is constant. Also, because the intersection
-- might change the order of the 'ProbChord's we sort the list again
-- descendingly
intersectPC :: [ProbChord] -> [ProbChord] -> [ProbChord]
intersectPC a b = reverse . sortBy (comparing prob)
                          $ mapMaybe (findAndMerge a') b' where
  (a',b') = order a b
  -- N.B. the probabilities are not divided by their length so > 1,
  -- due to the addition
  findAndMerge :: [ProbChord] -> ProbChord -> Maybe ProbChord
  findAndMerge pcs pc = case find (== pc) pcs of
    (Just pc') -> Just (ProbChord (chordLab pc) (prob pc + prob pc'))
    Nothing    -> Nothing

  -- takes two lists and returns a tuple where the first element is the smallest
  -- and the second element is the largest of the two lists
  order  :: [a] -> [a] -> ([a],[a])
  order x y
    | length x <= length y = (x,y)
    | otherwise            = (y,x)

--------------------------------------------------------------------------------
-- Matrix Functions for matching chords
--------------------------------------------------------------------------------

-- | Having a matrix of beat-synchronised bass and treble chromagrams and a
-- chord dictionary, the probability of a chord sounding at a particular beat is
-- estimated by calculating the Euclidean distance between the chord structures
-- and the chroma feature. These distances are calculated for every chord
-- candidate at every beat. Next, we sort the chord candidates by descending
-- Euclidean distance. To obtain a relative measure of the fit
-- between a chord candidate and the chroma vector in the range [0,1],
-- the distances are normalised by dividing them by distance of the best
-- matching chord candidate.
createChordRanks :: [BeatChroma] -> [TimedData [ProbChord]]
createChordRanks =
  map (selectTop . normalize . sortTake . matchCDictionary <$>) . meanBeatSyncVectors
    where
    -- here prob is still (unnormalised) euclidean distance
    sortTake, normalize :: [ProbChord] -> [ProbChord]
    sortTake          = take maxProbChordListLength . sortBy (comparing prob)

    normalize l@(h:_) = let ph = prob h in map (\p -> p{prob = ph / prob p }) l
    normalize []      = []

    selectTop l -- selects the everything with a probability > x
      | null s       = none
      -- so far, this had not positive effect
      --   length s > maxProbChordListLength = none
      | otherwise    = s
          where s    = takeWhile ((> cutOffProbability) . prob) l
                none = [ProbChord noneLabel 0.0]

-- takes the mean of every "beat block" and these Vectors as one Matrix
-- Each row of this matrix corresponds to the chroma within one beat
-- N.B. BeatChroma = BeatTimeData ChordinoLine
meanBeatSyncVectors :: [BeatChroma] -> [TimedData (Vector NumData)]
meanBeatSyncVectors = map (meanMat <$>) . beatSyncMatrix

-- creates a list of matrices, in which each matrix corresponds to the
-- collection of chroma vectors within one beat (drop the time stamp)
beatSyncMatrix :: [BeatChroma] -> [TimedData (Matrix NumData)]
beatSyncMatrix = map (toChromaMatrix <$>)

-- converts a ChordinoData into a Matrix
toChromaMatrix :: [ChordinoLine] -> Matrix NumData
toChromaMatrix = fromLists . map mergeLine where
  mergeLine :: ChordinoLine -> [NumData]
  mergeLine (ChordinoLine tm bs tb) = bs ++ tb

-- matches all transpositions of a chord structure with a chroma vector
matchCDictionary :: Vector NumData -> [ProbChord]
matchCDictionary v = map (matchStruct v) chordDictionary

-- calculate an Euclidean (PNorm2) norm
-- I also tried using the maximum norm, but this gave inferior results
matchStruct :: Vector NumData -> ChordCand -> ProbChord
matchStruct chroma (ChordCand r _ir None cs) =
  ProbChord (Chord r None [] 0 1) (norm2 (chroma - fromList (cs ++ cs)))
matchStruct chroma (ChordCand r _ir sh cs) =
  -- Chord root shorthand degrees location duration
  -- ProbChord (Chord r sh [] 0 1) (sqrt (bss * bss + treble * treble))
  ProbChord (Chord r sh [] 0 1) ((bss + treble) * 0.5)
    where treble = norm2 (slice 12 12 chroma - fromList cs)

          (_ir,bss) = matchInv r

          -- calculates the euclidean distance between the bass chromagram
          -- and all bass note inversions.
          matchInv :: Root -> (Root, NumData)
          matchInv ir = (ir, norm2 (slice 0 12 chroma - bcs)) where
            bcs         = fromList (pre ++ [1.0] ++ tail post)
            (pre, post) = splitAt (toSemitone ir) (shortHandToCS None)

          -- For a given chord structure, compute all possible
          -- roots for inversions
          -- rootInvs :: ChordStruct -> [Root]
          -- rootInvs cs = map iThRoot (elemIndices 1 cs)

--------------------------------------------------------------------------------
-- The Chord Dictionary
--------------------------------------------------------------------------------

-- the chord dictionary of all chords that are matched
chordDictionary :: [ChordCand]
chordDictionary = concatMap transpose12 [minBound..] -- all shorthands :-)

-- takes a ChordStruct and returns a list containing the 12 transposed versions
-- (including the original) of the ChordStruct, times the number of possible
-- inversions (3 or 4, depending on the number of notes in the shorthand).
transpose12 :: Shorthand -> [ChordCand]
transpose12 sh
  | null cstruct = []
  | sh == None   = [ChordCand (Note Nothing N) (Note Nothing N) None cstruct]
  | otherwise    = concatMap mkChordCands [0..11]

  where
      cstruct :: ChordStruct
      cstruct = shortHandToCS sh

      mkChordCands :: Int -> [ChordCand]
      mkChordCands n = [ ChordCand r r sh shiftedCS]
                       where shiftedCS = shift (12-n) cstruct
                             r         = toRoot n

shortHandToCS :: Shorthand -> ChordStruct
shortHandToCS sh = case sh of
  --        0,  1,  2,  3,   4,  5,  6,  7,  8,  9,  10,  11
  --          C, Db,  D,    Eb,    E,  F, F#,   G, Ab,  A,   Bb,   B
  Maj   -> [1.5,  0,  0.5,   0,  1.0,  0,  0, 1.5,  0,  0.5,   0,  0.5 ]
  Min   -> [1.5,  0,  0  , 1.0,  0  ,  0,  0, 1.5,  0,  0  , 1.0,  0   ]
  Sev   -> [1.5,  0,  0  ,   0,  1.0,  0,  0, 1.5,  0,  0.0, 1.0,  0   ]
  -- Dim   -> [1,  0,  0,  1,  0,  0,  1,  0,  0,  0,  0,   0 ]
  -- HDim7 -> [1,  0,  0,  1,  0,  0,  1,  0,  0,  0,  1,   0 ]
  -- Dim   -> [1,  0,  0,  1,  0,  0,  1,  0,  0,  0,  0,   0 ]
  -- Maj6  -> [1,  0,  0,  0,  1,  0,  0,  1,  0,  1,  0,   0 ]
  -- Dim7  -> [1,  0,  0,  1,  0,  0,  1,  0,  0,  1,  0,   0 ]
  -- Maj7  -> [1,  0,  0,  0,  1,  0,  0,  1,  0,  0,  0,   1 ]
  -- Min7  -> [1,  0,  0,  1,  0,  0,  0,  1,  0,  0,  1,   0 ]
  -- Min6  -> [1,  0,  0,  1,  0,  0,  0,  1,  0,  1,  0,   0 ]
  -- Sus4  -> [1,  0,  0,  0,  0,  1,  0,  1,  0,  0,  0,   0 ]

  None  -> [0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,   0 ]
  -- None -> [1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,   1 ]
  _     -> []
  -- none
  -- all
  -- ....

--------------------------------------------------------------------------------
-- general Matrix stuff
--------------------------------------------------------------------------------

-- given a matrix, calculates the mean vector
meanMat :: forall t. (Fractional t, Num t, Show t) => Matrix t -> Vector t
meanMat m = generate (ncols m) vmean where
  vmean :: Int -> t
  vmean i = mean (getCol (i+1) m)

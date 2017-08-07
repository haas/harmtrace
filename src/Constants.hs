{-# OPTIONS_GHC -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Constants
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Several constants used everywhere
--------------------------------------------------------------------------------

module Constants ( vERSION, cutOffProbability, maxProbChordListLength
                 , maxSegmentSize, maxLProductSize, evaluationSampleRate
                 , displaySampleRate, outputSampleRate, outputBitsPerSample
                 , outputNrOfChannels, keyStr, chromaStr, beatStr
                 , beatVampPath, chromaVampPath, keyVampPath
                 , deleteDownsampledWav, downSample, defaultLogDir
                 , defaultVampDir, defaultFeatDir , defaultOutDir, logFileSuffix
                 , sonicAnnotator, modulationPenalty, minModulationLength
                 , ChordPrintOpts (..), defaultChordPrinting ) where

import HarmTrace.Base.MusicTime (NumData)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Automatically generated
vERSION :: String
vERSION = gitVersion ++ " (master)"

gitVersion :: String
gitVersion = "HarmTrace-2.2.0"

--------------------------------------------------------------------------------
-- Chord Transcription Parameters
--------------------------------------------------------------------------------

-- | The cutOffProbability is the value that determines the length of the
-- probChord list (a sorted list with normalised distances to the chroma vector)
cutOffProbability      :: NumData
cutOffProbability      = 0.85

maxProbChordListLength, maxSegmentSize, maxLProductSize :: Int
-- | The maximum number if chords (the lenght) of a chord candidate list
maxProbChordListLength = 6

-- | The maximum size of a list of chord candidate lists (of which all possible
-- combinations will be parsed). If, in the segmentation procedure, this
-- threshold is crossed an 'emergency split' will be made (see
-- Harmtrace.Audio.Annotate.emergencySplit for details)
maxSegmentSize         = 12

-- | Chord candidate list segments can contain long and short chord candidate
-- lists. This strongly influences the number of considered chord sequence
-- combinations, i.e. the LProductSize. If the number of possible combinations
-- becomes greater than the 'maxLProductSize' also an 'emergency split' will
-- be made, similar to maxSegmentSize.
maxLProductSize        = 30

--------------------------------------------------------------------------------
-- Evaluation Parameters
--------------------------------------------------------------------------------

evaluationSampleRate, displaySampleRate :: NumData
-- | The sample rate used in a normal (non-visual) comparison (in seconds).
evaluationSampleRate = 0.01
-- | The sample rate used when visually comparing a chord annotation with a
-- ground-truth annotation. Often a higher sample rate is preferred. Although
-- one uses precision, the visual result is easier to read.
displaySampleRate    = 0.3


--------------------------------------------------------------------------------
-- Feature Extraction and Downsampling Parameters
--------------------------------------------------------------------------------

outputSampleRate, outputBitsPerSample, outputNrOfChannels :: Int
-- | Before feature extraction, all audio is downsampled to 'outputSampleRate'
-- Hz. N.B. If the sample rate is changed the VAMP plugin transforms should be
-- updated accordingly (see 'beat', 'chroma', 'key').
outputSampleRate    = 22050

-- | Before feature extraction, all audio is converted to contain
-- 'outputBitsPerSample' bits per sample.
outputBitsPerSample = 16

-- | Before feature extraction, all audio is converted to contain a fixed number
-- of channels, e.g. 1 for mono or 2 for stereo.
outputNrOfChannels  = 1

-- | Controls whether the downsampled wav file is deleted after feature
-- extraction.
deleteDownsampledWav, downSample :: Bool
deleteDownsampledWav = True

-- | Controls whether SoX is used to downsample the audio or whether the
-- audio file is passed to the sonic annotator directly. If 'downSample is
-- set to False, all paramters downsampling parameters will be ignored
downSample = True

-- | The strings that build up a audio feature file name
keyStr, chromaStr, beatStr :: String
chromaStr = "_vamp_nnls-chroma_nnls-chroma_bothchroma.csv"
keyStr    = "_vamp_nnls-chroma_nnls-chroma_chroma.csv"
-- beatStr   = "_vamp_qm-vamp-plugins_qm-tempotracker_beats.csv"
beatStr   = "_vamp_qm-vamp-plugins_qm-barbeattracker_beats.csv"

beatVampPath, chromaVampPath, keyVampPath :: FilePath
-- | specifying the filename of the beat tracker VAMP plugin transform file
beatVampPath   = "beat-tracker-one.txt"
-- | specifying the filename of the NNLS chroma VAMP plugin transform file
chromaVampPath = "both-chroma.txt"
-- | specifying the filename of the key-finding chroma VAMP plugin transform file
keyVampPath    = "tuned-chroma.txt"

defaultVampDir, defaultLogDir, defaultOutDir, defaultFeatDir :: FilePath
-- | The default base directory is the current directory
defaultVampDir = ""
defaultOutDir  = ""
defaultLogDir  = ""
defaultFeatDir = ""

-- | By default we assume the sonic-annotator is in the path
sonicAnnotator, logFileSuffix :: String
sonicAnnotator = "sonic-annotator"
logFileSuffix  = "extract.log"

-- | An ennumerator of determining the output format of the chords
data ChordPrintOpts = PrintMajMin | PrintChordClass

-- | By default we only display major and minor chords to the user (because
-- the transcription quality of these chords is better than that of the
-- chord class).
defaultChordPrinting :: ChordPrintOpts
defaultChordPrinting = PrintMajMin

--------------------------------------------------------------------------------
-- Chroma key estimation
--------------------------------------------------------------------------------

-- | The penatly given in the cumulative key strength calculation. For every
-- beat the correlation between a Krumhansl profile and the current beat
-- synchronised chroma feature is calculated for all 24 keys at every beat.
-- An optimal path through this table can be defined as:
-- $M[i,j] = max \{ M[i-1,j] + K[i,j],
--                  M[i-1,j] + K[i,k] + p, }
--                 where \{k \mid \forall x : K[i,x] \leq K[i,k]\}$
-- where $p$ is the modulationPenalty, i.e. the penalty for changing the key
-- at that particular beat.
modulationPenalty :: NumData
modulationPenalty   = 1.0

-- | The minimal length in beats of a key segment
minModulationLength :: Int
minModulationLength = 16

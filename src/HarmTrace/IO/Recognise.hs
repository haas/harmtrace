{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.IO.Recognise
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Handels all the IO of the 'recognise' mode of HarmTrace, which
-- deals with audio chord transcription.
--------------------------------------------------------------------------------

module HarmTrace.IO.Recognise (

      evaluateLabeling, printLabelling, batchPrintLabelling, batchLabeling
    , batchEvaluateKeyFinding, evaluateKeyFinding

  ) where


-- Common IO functions
import HarmTrace.IO.Common
import HarmTrace.IO.BasePaths (BasePaths, getOutDir, getFeatDir, getLogFile)
import Constants ( keyStr, chromaStr, beatStr
                 , ChordPrintOpts (..) )

-- Music stuff
import HarmTrace.Base.MusicRep
import HarmTrace.Models.Jazz.Instances ()
import HarmTrace.Base.Parsing hiding ((<.>))
import HarmTrace.Models.Models 

-- Audio stuff
import HarmTrace.Audio.DataParser ( parseChordinoData, parseChromaData
                                  , parseBarTimeData)
import HarmTrace.Audio.AnnotationParser
import HarmTrace.Audio.Annotate (putSegStats, mptreeAnnotator)
import HarmTrace.Audio.Evaluation ( relCorrectOverlap, achievScore, avgDistToOne
                                  , chordTriadEq, chordClassEq, majMinEq
                                  , printChordRCO, printRCO, chordChangeRatio )
import HarmTrace.Base.MusicTime
import HarmTrace.Audio.ChromaChord ( createChordRanks, beatSync )
import HarmTrace.Audio.Key (getBeatSyncKeyFromChroma)
import HarmTrace.IO.FeatExtract
import Data.List (genericLength, isSuffixOf, stripPrefix)

-- Library modules
import Data.List (sort, unzip6, intersperse)
import System.FilePath
import System.Directory
import System.IO
import System.CPUTime
import Data.Maybe (isJust, fromJust, isNothing)
import Text.Printf (printf)
-- import Control.Parallel.Strategies (parList, rdeepseq, using)

--------------------------------------------------------------------------------
-- Some audiofile specific utitlities
-------------------------------------------------------------------------------- 

-- | Returns True when the argument is an audio feature file based on 
-- the filename
isAudioFeatureFile :: FilePath -> Bool
isAudioFeatureFile fp
  | isSuffixOf chromaStr fp = True
  | isSuffixOf keyStr    fp = True
  | isSuffixOf beatStr   fp = True
  | otherwise               = False

-- | Retuns the audio feature identifier, which equals the filename without
-- the extension
getAudioFeatureId :: FilePath -> Maybe String
getAudioFeatureId fp
  | isJust key  = key
  | isJust chm  = chm
  | isJust bt   = bt  
  | otherwise   = Nothing
  where key  = stripSuffix keyStr fp
        chm  = stripSuffix chromaStr fp
        bt   = stripSuffix beatStr fp

-- drops a given suffix from a string. It returns nothing if the suffix 
-- is not a suffix of the string
stripSuffix :: String -> String -> Maybe String
stripSuffix suf txt 
  | isJust stp = Just . reverse $ fromJust stp
  | otherwise  = Nothing
  where stp = stripPrefix (reverse suf) (reverse txt) 

-- maps readAudioFeat over a directory
readAudioFeatureDir :: FilePath -> IO [AudioFeat]
readAudioFeatureDir fp = 
  do fs <- getDirectoryContents fp
     mapM readAudioFeatures (group . sort $ filter isAudioFeatureFile fs) 
       where
       group :: [FilePath] -> [(FilePath, FilePath, FilePath)]
       group (c:k:b:fs) = (fp </> c, fp </> b, fp </> k) : group fs
       group [] =[]
       group _  = error ("HarmTrace.IO.Recognise.readAudioFeatureDir: the " 
                   ++  "number of files in the filepath cannot be divided by 3")

-- Given triplet of three filenames describing 
-- a chroma, beat and key feature file, 'readAudioFeat' parses all data and returns an 'AudioFeat'.
readAudioFeatures :: (FilePath,FilePath,FilePath) -> IO (AudioFeat)
readAudioFeatures (cfp,bfp,kfp) =
  do let bfpid = getAudioFeatureId bfp 
         cfpid = getAudioFeatureId cfp 
         kfpid = getAudioFeatureId kfp 
     when (bfpid /= cfpid && bfpid /= kfpid && isJust bfpid) 
          (error ("HarmTrace.IO.readAudioFeatures: non matching audio " ++ 
                  "features:\n" ++ show bfp ++ "\n" ++ show cfp ++ "\n" ++ 
                  show kfp ++ "\n"))
     
     dChroma <- readFile cfp
     dBeat   <- readFile bfp
     dKey    <- readFile kfp
     
     -- TODO: use parseDataSafe here
     let chrm  = parseData parseChordinoData    dChroma
         beats = parseData parseBarTimeData     dBeat
         keys  = parseData parseChromaData      dKey
     return (AudioFeat chrm beats keys (fromJust bfpid))

-- Given one VAMP feature CSV file, HarmTrace will look for the other two 
-- feature files needed for chord transcription. If the first argument it True
-- 'findAudioFeatures' outputs the filepaths of the feature files it found. 
findAudioFeatures :: Bool -> FilePath -> IO (Maybe (FilePath,FilePath,FilePath))
findAudioFeatures verbose fp =
  case getAudioFeatureId fp of
    Nothing     -> return Nothing
    (Just afid) -> do let cfp = afid ++ chromaStr
                          bfp = afid ++ beatStr
                          kfp = afid ++ keyStr
                     
                      cfpExists <- doesFileExist cfp
                      bfpExists <- doesFileExist bfp
                      kfpExists <- doesFileExist kfp
                      
                      when (verbose && cfpExists) (putStrLn ("found: " ++ cfp))
                      when (verbose && bfpExists) (putStrLn ("found: " ++ bfp))
                      when (verbose && kfpExists) (putStrLn ("found: " ++ kfp))
                      
                      case (cfpExists && bfpExists && kfpExists) of
                         True  -> return (Just (cfp,bfp,kfp))
                         False -> 
                           do when verbose (putStrLn ("I did not find 3 "
                                   ++ "matching feature files, I looked for:\n"
                                   ++ cfp ++ "\n" ++ bfp ++ "\n" ++ kfp))
                              return Nothing
                
-- | Returns True if fp is an audio file
isAudioFile :: FilePath -> Bool
isAudioFile fp = elem (tail $ takeExtension fp) -- all sox file formats :-)
  [ "mp3", "wav", "mp4","MP3", "MP4","WAV"      -- start with common extensions
  , "8svx", "aif", "aifc", "aiff", "aiffc", "al", "amb", "amr-nb", "amr-wb" 
  , "anb", "au", "avi", "avr", "awb", "cdda", "cdr", "cvs", "cvsd", "cvu", "dat"
  , "dvms", "f32", "f4", "f64", "f8", "ffmpeg", "flac", "fssd", "gsm", "gsrt"
  , "hcom", "htk", "ima", "ircam", "la", "lpc", "lpc10", "lu", "m4a", "m4b"
  , "maud", "mp2", "mpg", "nist", "ogg", "prc", "raw", "s1"
  , "s16", "s2", "s24", "s3", "s32", "s4", "s8", "sb", "sds", "sf", "sl", "smp"
  , "snd", "sndfile", "sndr", "sndt", "sou", "sox", "sph", "sw", "txw", "u1"
  , "u16", "u2", "u24", "u3", "u32", "u4", "u8", "ub", "ul", "uw", "vms", "voc"
  , "vorbis", "vox",  "wavpcm", "wmv", "wv", "wve", "xa"
  -- and the same extensions in uppercase
  -- TODO: use equalFilePath 
  , "8SVX", "AIF", "AIFC", "AIFF", "AIFFC", "AL", "AMB", "AMR-NB", "AMR-WB" 
  , "ANB", "AU", "AVI", "AVR", "AWB", "CDDA", "CDR", "CVS", "CVSD", "CVU", "DAT"
  , "DVMS", "F32", "F4", "F64", "F8", "FFMPEG", "FLAC", "FSSD", "GSM", "GSRT"
  , "HCOM", "HTK", "IMA", "IRCAM", "LA", "LPC", "LPC10", "LU", "M4A", "M4B"
  , "MAUD", "MP2",  "MPG", "NIST", "OGG", "PRC", "RAW", "S1"
  , "S16", "S2", "S24", "S3", "S32", "S4", "S8", "SB", "SDS", "SF", "SL", "SMP"
  , "SND", "SNDFILE", "SNDR", "SNDT", "SOU", "SOX", "SPH", "SW", "TXW", "U1"
  , "U16", "U2", "U24", "U3", "U32", "U4", "U8", "UB", "UL", "UW", "VMS", "VOC"
  , "VORBIS", "VOX",  "WAVPCM", "WMV", "WV", "WVE", "XA"
  ]
  
--------------------------------------------------------------------------------
-- Evaluating Audio Chord Transcription
--------------------------------------------------------------------------------
 
-- | Evaluates a single labeling of a piece with a ground truth annotation 
-- visually.
evaluateLabeling :: (Maybe [TimedData Key] -> AudioFeat -> ChordAnnotation) 
                 -> Bool -> FilePath -> FilePath -> Maybe FilePath -> IO Double
evaluateLabeling annotator prnt gtfp featfp keyfp = do
  
  maf <- findAudioFeatures True featfp
  af  <- readAudioFeatures (fromJust maf)
  gt  <- readAnnotation gtfp
  
  case (isJust maf, keyfp, prnt) of
    (True, Nothing,True)  -> 
        do printLn ("using key finding")
           putSegStats Nothing af
           printChordRCO  (annotator Nothing) (getBeatSyncKeyFromChroma af) af gt
    (True, Nothing,False) -> 
        do return (relCorrectOverlap chordTriadEq gt (dropProb $ annotator Nothing af))
    (True, Just k ,True)  -> 
        do key <- readAndParseKeyAnn k
           printLn ("using groundTruth key annotation: " ++ show key)
           putSegStats (Just key) af
           printChordRCO (annotator (Just key)) key af gt
    (True, Just k ,False) -> 
        do key <- readAndParseKeyAnn k
           return (relCorrectOverlap chordTriadEq gt (dropProb $ annotator (Just key) af))
    (False, _, _)         -> return (-1)

-- | Evaluates the keyfinding for a single audio feature set. 
evaluateKeyFinding :: Bool -> FilePath -> FilePath -> IO ()
evaluateKeyFinding prnt featfp kdir = do

  maf <- findAudioFeatures True featfp
  af  <- readAudioFeatures (fromJust maf)
  
  gt <- readAndParseKeyAnn (kdir </> takeFileName (getAudioFeatId af) <.> "lab")
  let keys = getBeatSyncKeyFromChroma af
  
  case (prnt) of
    True  -> void (printRCO printKeyEq gt keys)
    False -> putStrLn (show $ relCorrectOverlap (==) gt keys)
    
  where printKeyEq :: Key -> Key -> IO (Bool)
        printKeyEq a b = do putStrLn (show a ++ " == " ++ show b)
                            return (a == b)
    
  
-- | Given a ground truth directory and an data directory (containing exactly
-- 3 times as much files as the gt directory) all files will be labeled and
-- the relative correct overlap wil be corrected an presented to the user
batchLabeling ::  ChordPrintOpts ->  Maybe FilePath ->  
              (Maybe [TimedData Key] -> AudioFeat -> ChordAnnotation) 
              -> FilePath -> FilePath   -> Maybe FilePath -> IO ()
batchLabeling opts mout annotator gtdir audiodir mkeydir = do
  
  -- inform the user wheter we are using ground-truth key annotations or not
  maybe (putStrLn "using key finding") 
        (const $ putStrLn "using key ground-truth annotations")  mkeydir
    
  printVersion -- print the current HarmTrace version
  printLn ("file\trun time (seconds)\tRCO triad (maj/min)\tmaj/min errors" ++ 
           "\tRCO chord class (maj/min/dom/dim)\tMaximum achievable"       ++
           "\tchord change ratio") 
  
  result <- readAudioFeatureDir audiodir >>= mapM evalR 
  
  let -- result' = result `using` parList rdeepseq
      totNrSongs = genericLength result
      printAVG r = show (sum r / totNrSongs)
      (triad, majMinErrs, chordClass, maxAchieve, cChangeRat, runTimes) = unzip6 result
  -- print some averaged statistics
  putStrLn ("average RCO triad level: " ++ printAVG triad) 
  putStrLn ("average MajMin error ratio: " ++ printAVG majMinErrs) 
  putStrLn ("average RCO chord class level: " ++ printAVG chordClass)
  putStrLn ("average maximum achievable score: " ++ printAVG maxAchieve)
  putStrLn ("average chord change ratio (distance to 1.0): " 
                                        ++(show . avgDistToOne $ cChangeRat))
  putStrLn ("total running time: "      ++ show (sum runTimes ))
  
  where
    -- evaluates one set of audio features, prints the results to the std. out
    -- and returns a tuple with the relative correct overlap and the run time
    evalR :: AudioFeat -> IO(Double, Double, Double, Double, Double, Double)
    evalR af@(AudioFeat crm bts _ afid) = do
    
      let gtfp = takeFileName afid <.> "lab"
      gt <- readAnnotation (gtdir </> gtfp)
      
      -- if a directory with ground-truth key annotations has been provided
      -- we use this directory to find a file with the same name in this 
      -- directory and parse it
      mkey <- case mkeydir of
                Just kfp -> readAndParseKeyAnn (kfp </> gtfp) >>= return . Just
                Nothing  -> return Nothing       
               
      let timedAnno   = annotator mkey af
          annotation  = dropProb timedAnno
          resultTriad = relCorrectOverlap chordTriadEq gt annotation
          resultClass = relCorrectOverlap chordClassEq gt annotation
          majMinErr   = relCorrectOverlap majMinEq     gt annotation
          chordChange = chordChangeRatio chordTriadEq  gt annotation
          ccList      = fmap (fmap (fmap chordLab)) . createChordRanks
                      $ beatSync bts crm
          resultMax   = achievScore gt ccList
          exec        = seq annotation (return ())
          
      -- TODO: refactor into seperate function
      -- log the execution time
      t1 <- getCPUTime
      exec -- evaluate the automatic annotation
      t2 <- getCPUTime
      
      -- optionally write the chords to an output file (if an output directory
      -- has been specified)
      when (isJust mout) 
           (writeAnnotationNoLog opts (fromJust mout) afid timedAnno)
      
      let runtime = fromIntegral (t2 - t1) / (1000000000000 :: Double) 
      
      -- print the information to the user
      printLn (gtfp ++ '\t' : pPrintDoubles [runtime, resultTriad
                  , majMinErr, resultClass,  resultMax, chordChange])
      return (resultTriad, majMinErr, resultClass, resultMax, chordChange, runtime)

-- | Pretty prints a list of doubles separated by tabs.
pPrintDoubles :: [Double] -> String
pPrintDoubles = concat . intersperse "\t" .  map (printf "%.3f")
      
-- | reads a single chord annotation
readAnnotation :: FilePath -> IO [TimedData ChordLabel]
readAnnotation fp = do f <- readFile fp
                       return (parseDataSafe parseAnnotationData f)

-- | Given a 'FilePath' reads and returns a ground-truth 'Key' Annotation.
readAndParseKeyAnn :: FilePath -> IO [TimedData Key]
readAndParseKeyAnn keyfp = do key <- readFile keyfp 
                              return $ parseDataSafe parseKeyAnnotationData key

-- | Evaluates key finding based on a directory of audio features
batchEvaluateKeyFinding :: FilePath -> FilePath ->  IO ()
batchEvaluateKeyFinding audiodir keydir = do
  putStrLn "Evaluating key finding"
  printVersion -- print the current HarmTrace version
  
  result <- readAudioFeatureDir audiodir >>= mapM evalKey 
  
  let totNrSongs     = genericLength result
      (roc,runTimes) = unzip result
  -- print some averaged statistics
  putStrLn ("average RCO: "        ++ show (sum roc / totNrSongs )) 
  putStrLn ("total running time: " ++ show (sum runTimes )) 
  where
  
    -- Evaluates a the key annotation for a single AudioFeature set 
    evalKey :: AudioFeat -> IO (Double, Float)
    evalKey af = do
    
      let gtfp = takeFileName (getAudioFeatId af) <.> "lab"
      gt <- readAndParseKeyAnn (keydir </> gtfp)
      
      let result = relCorrectOverlap (==) gt (getBeatSyncKeyFromChroma af)
          exec   = seq result (return ())
             
      -- log the execution time
      t1 <- getCPUTime
      exec -- evaluate the automatic annotation
      t2 <- getCPUTime
      let runtime = fromIntegral (t2 - t1) / (1000000000000 :: Float) 
      
      -- print the information to the user
      printLn (gtfp ++ ":\t" ++ showFloat runtime ++  '\t'  : show result )
      return (result, runtime)
     

--------------------------------------------------------------------------------
-- Analysing and printing chord labels
--------------------------------------------------------------------------------

batchPrintLabelling :: ChordPrintOpts -> BasePaths 
              -> (Maybe [TimedData Key] -> AudioFeat -> ChordAnnotation) 
              -> FilePath -> IO ()
batchPrintLabelling opts dirs annotator fp =               
  do fl <- readFile fp
     mapM_ (printLabelling opts dirs annotator) (lines fl)
                     
-- takes a set of features or an audio file and writes the chords to a file
-- the process is also logged in a logfile to keep track of the process
-- (see HarmTrace.IO.FeatExtract)
printLabelling :: ChordPrintOpts -> BasePaths
               -> (Maybe [TimedData Key] -> AudioFeat -> ChordAnnotation) 
               -> FilePath -> IO ()
printLabelling opts dirs annotator fp = do
  let -- we use the same logfile for adding harmtrace status
      logf = getLogFile dirs fp
      out  = getOutDir  dirs </> takeFileName fp <.> "chords.txt" 
      ffp  = getFeatDir dirs </> dropExtension (takeFileName fp) ++ chromaStr
  
  -- check (silently) if features have been extracted earlier
  hasFt <- findAudioFeatures False ffp

  case (isAudioFile fp, hasFt) of
    -- We found an audio file and no previously extracted features: extract them
    (True, Nothing) -> do -- extract features
                          maf <- extractFeatures dirs fp
                          hdl <- openFile logf AppendMode
                          if isNothing maf
                             then harmTraceLogFinal hdl fp "error"
                             else do readAndPrint opts hdl fp out maf annotator
                                     harmTraceLogFinal hdl fp "done"
                                     hClose hdl
                                  
    -- We found an audio file, but also found matching features
    (True,Just _  ) -> do putStrLn ("I found matching feature files for " ++ fp)
                          hdl <- openFile logf WriteMode
                          readAndPrint opts hdl fp out hasFt annotator 
                          harmTraceLogFinal hdl fp "done"
                          hClose hdl
    
    -- No audio, but one feature file: look for all feature files
    (False,_      ) -> do maf <- findAudioFeatures True fp -- N.B. will print
                          hdl <- openFile logf WriteMode
                          if isNothing maf
                             then harmTraceLogFinal hdl fp "error"
                             else do readAndPrint opts hdl fp out maf annotator
                                     harmTraceLogFinal hdl fp "done"
                                     hClose hdl

      
-- reads the features and prints the chords
readAndPrint :: ChordPrintOpts -> Handle -> FilePath -> FilePath 
             -> Maybe (FilePath, FilePath, FilePath) 
             -> (Maybe [TimedData Key] -> AudioFeat -> ChordAnnotation) 
             -> IO ()
readAndPrint _    _    _   _    Nothing  _         = return ()
readAndPrint opts logh src out (Just af) annotator = 
  do feat <- readAudioFeatures af
     -- Compute the length of the input list of beats, to be used to provide
     -- progress information `writeAnnotation`
     let len = genericLength . getBeats $ feat
     writeAnnotation logh len src out . expandPerBeat 
                . applyPrintOpts opts $ annotator Nothing feat


-- | writes an annotation to a specific file
writeAnnotation :: Handle -> Float -> FilePath -> FilePath 
                -> ChordAnnotation -> IO ()
writeAnnotation hlog len src out ca = 
  do hout <- openFile out ReadWriteMode -- perhaps use WriteMode?
     let step = 100 / len
         -- showLn a = show a ++ "\n"
     mapWithStatus (hPutStr hout . showTDPC) (harmTraceLogger hlog src) step ca
     hClose hout 

-- | Does the same thing as writeAnnotation (storing a chord transcription),
-- but without the logging.
writeAnnotationNoLog :: ChordPrintOpts -> FilePath -> FilePath 
                     -> [TimedData ProbChord] -> IO ()
writeAnnotationNoLog opts dir srcfp ca = 
  do let outfp = dir </> (takeFileName srcfp) <.> "chords.txt"
     exists <- doesDirectoryExist dir
     if exists 
         then writeFile outfp . concatMap showTDPC 
                              . expandPerBeat . applyPrintOpts opts $ ca
         else hPutStr stderr (dir ++ " does not exits") 
     
-- Shows chords in such a way that Chordify can understand them
showTDPC :: Show a => TimedData a -> String
showTDPC td = concat . intersperse ";" $ [ show . getBeat $ td
                                         , show . getData $ td
                                         , show . onset $ td
                                         ,(show . offset $ td) ++ "\n" ]

-- like mapM_, but also print progress information
mapWithStatus :: (a -> IO b) -> (Float -> IO()) -> Float -> [a] -> IO ()
mapWithStatus f logger step l = 
  let statusEval e b cur = logger cur >> f e >> (b (step + cur))
  in  foldr statusEval (const (return ())) l 0

-- Writes a harmtrace status indicator to a log file in the same format
-- as 'HarmTrace.FeatExtract.FeatExtract'. 
harmTraceLogger :: RealFrac a => Handle -> FilePath -> a -> IO ()
harmTraceLogger hdl f step = 
  do let fstep = floor step :: Integer
         total = 70 + (fstep * 30 `div` 100) 
     hPutStr hdl (f ++ ";harmtrace;" ++ show fstep ++ ';' : show total ++ "\n")

-- Writes a final (100 percent) status message, should be "done" or "error"
harmTraceLogFinal :: Handle -> FilePath -> String -> IO ()
harmTraceLogFinal hdl f str = 
  hPutStr hdl (f ++ ";" ++ str ++";100;100\n")

applyPrintOpts :: ChordPrintOpts -> ChordAnnotation -> ChordAnnotation
applyPrintOpts PrintMajMin     cs = map (updateTPChord toMajMinChord) cs
applyPrintOpts PrintChordClass cs = cs

-- Expands the previously merged 'TimedData ProbChord's
-- TODO perhaps move to HarmTrace.Base.MusicTime because of it general structure
expandPerBeat :: [TimedData a] -> [TimedData a]
expandPerBeat = concatMap expand where
  
  expand :: TimedData a -> [TimedData a]
  expand td = let ts = getTimeStamps td
              in zipWith3 timedDataBT (repeat . getData $ td) ts (tail ts)

--------------------------------------------------------------------------------
-- for testing in ghci
--------------------------------------------------------------------------------

mptree :: FilePath -> IO [TimedData ProbChord]
mptree f = do maf <- findAudioFeatures True f
              case maf of
                Nothing -> error ("invalid audiofeature file " ++ show f) 
                Just af -> do readAudioFeatures af >>= 
                                return . mptreeAnnotator (GrammarEx Pop) Nothing

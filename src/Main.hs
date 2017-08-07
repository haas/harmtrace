{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Top-level file
--------------------------------------------------------------------------------

module Main where

import System.Console.CmdArgs.Explicit hiding (Arg)

import Constants (vERSION, defaultChordPrinting)

import HarmTrace.HarmTrace
import HarmTrace.IO.Main
import HarmTrace.IO.Errors
import HarmTrace.IO.PrintTree
import HarmTrace.IO.BasePaths (setPaths)
import HarmTrace.HAnTree.ToHAnTree (gTreeHead)
import HarmTrace.Matching.Standard
import HarmTrace.Matching.GuptaNishimura (getLCES)
import HarmTrace.Matching.Alignment (getAlignDist, alignChordLab
                                    , pPrintV, alignHAnChord)
import HarmTrace.Audio.Annotate     ( simpleAnnotator, groupAnnotator
                                    , mptreeAnnotator, mptreeAnnotatorSTG)
import HarmTrace.Base.MusicTime-- (AudioFeat, ChordBeatAnnotation, TimedData)
import HarmTrace.Base.MusicRep (Key)

import Data.List (delete)
import System.FilePath (takeFileName)
import System.Exit (exitSuccess, exitFailure)
import System.Environment ( getArgs )
import Control.Monad (when)
import Data.List ( elemIndex )
import Data.Monoid ( mempty )


--------------------------------------------------------------------------------
-- Top-level main
--------------------------------------------------------------------------------
harmTrace :: Mode [MyArgs]
harmTrace = (modes "harmtrace" mempty "Harmonic Analysis and Retrieval of Music"
              [parseMode, matchMode, recogniseMode])
            { modeGroupFlags = toGroup (inputFlags : helpVerFlags) }
            
inputFlags :: Flag [MyArgs]
inputFlags = flagReq ["r"] (upd "ReadFlags") 
               "file" "File to read flags from"

main :: IO ()
main = do -- Get the original args
          cliArgs <- getArgs
          -- Check if we've got -r
          let rArgs = case elemIndex "-r" cliArgs of
                        -- We did, remove all the rest
                        Just n | length cliArgs > (n+1)
                          -> [cliArgs !! n, cliArgs !! (n+1)]
                        -- We did not, so all the flags come from the CLI
                        _ -> cliArgs
              strippedArgs = processValue harmTrace rArgs
          
          -- Check if we should get flags from a file
          -- NOTE: reading flags such as --chords="A B" will not work, because
          -- there are spaces inside the string "A B", and `words` isn't clever
          -- enough to ignore those.
          newArgs <- case (getArg strippedArgs "ReadFlags") of
                       -- Nope, use the original flags
                       Nothing -> return (processValue harmTrace cliArgs)
                       -- Yes, read them from the file and merge with the
                       -- received flags
                       Just f  -> do s <- readFile f
                                     return $ processValue harmTrace
                                                (words s ++ cliArgs)

          -- Check mode and redirect control to appropriate function
          case (getArg newArgs "Mode") of
            Just "Parse"     -> mainParse newArgs
            Just "Match"     -> mainMatch newArgs
            Just "Recognise" -> mainRecognise newArgs
            Just m           -> putStrLn ("Unknown mode: " ++ m)
            Nothing          -> -- Handle help and version, if present
                                handleHelpVer newArgs harmTrace

          -- If invoked without arguments, print help
          when (null cliArgs) $ print (helpText [] HelpFormatDefault harmTrace)

--------------------------------------------------------------------------------
-- Parse mode
--------------------------------------------------------------------------------
parseMode :: Mode [MyArgs]
parseMode = mode "parse" [Arg "Mode" "Parse"] "Parse files into harmonic analysis trees"
              (flagArg upd0 "")
              ([flagReq ["g", "grammar"] updG                          "string"   "Grammar to use (jazz|pop)"
               ,flagReq ["c", "chords"]  (upd "SourceInputString")     "string"   "Input chord sequence to parse"
               ,flagReq ["i", "file"]    (upd "SourceInputFile")       "filepath" "Input file to parse"
               ,flagReq ["d", "dir"]     (upd "InputDir")              "filepath" "Input directory to parse all files within"
               ,flagReq ["o", "out"]     (upd "BinaryOut")             "filepath" "Output binary file to write parse results to"
               -- We should add a flag for describing that when using -k we are
               -- reading files in a different input syntax...
               ,flagReq ["k", "key"]     (upd "SourceKeyInputFile")  "filepath" "Ground-truth key annotation file"
               ,flagReq ["x", "key-dir"] (upd "AnnotationKeyInputDir")   "filepath" "Ground-truth key annotation directory"
               ,flagNone ["p", "print"] ((Switch "Print"):)                       "Output a .png of the parse tree"
               ,flagNone ["s", "print-insertions"] ((Switch "PrintIns"):)         "Show inserted nodes"
               ] ++ inputFlags : helpVerFlags)

  where -- Flags without input
        upd0 "--print"            args = Right $ Switch "Print"    : args
        upd0 "-p"                 args = Right $ Switch "Print"    : args
        upd0 "--print-insertions" args = Right $ Switch "PrintIns" : args
        upd0 "-s"                 args = Right $ Switch "PrintIns" : args
        upd0 s                    _    = Left  $ "Unknown argument: " ++ s

mainParse :: [MyArgs] -> IO ()
mainParse args =
 do let cStr = getArg args "SourceInputString"
        sif  = getArg args "SourceInputFile"
        ky   = getArg args "SourceKeyInputFile"
        bOut = getArg args "BinaryOut"
        mdir = getArg args "InputDir"
        kdir = getArg args "AnnotationKeyInputDir"
        prnt = gotArg args "Print"
        opts = if gotArg args "PrintIns"
               then delete RemoveInsertions defaultOpts else defaultOpts

    -- Handle help and version, if present
    handleHelpVer args parseMode

    case (getGram args) of
      Nothing -> putStrLn "Please supply a grammar to use" >> exitFailure
      Just (GrammarEx g) ->
        case (cStr, sif,mdir, prnt, ky, kdir) of
          -- parse a string of chords
          (Just c, Nothing, Nothing, False, Nothing, Nothing)  ->
            do pr <- parseTreeVerb g opts c
               mapM_ (print . gTreeHead) (parsedPiece pr)
          -- and print a parsetree     
          (Just c, Nothing, Nothing, True, Nothing, Nothing)   ->
            do pr <- parseTree g opts c 
               let ts = map gTreeHead (parsedPiece pr)
               _ <- printTreeHAn (pieceTreeHAn pr) (trimFilename ("pp" ++ c))
               printTreeHAnF ts (trimFilename c) >> return ()
          -- Parse one file, show full output
          (Nothing, Just f1, Nothing, False, Nothing, Nothing) -> 
            do pr  <- readFile f1 >>= parseTreeVerb g opts
               print (pieceTreeHAn pr)
               mapM_ (print . gTreeHead) (parsedPiece pr)
          (Nothing, Just f1, Nothing , True, Nothing, Nothing) ->
          --with post processing
            do pr <- readFile f1 >>= parseTree g opts
               let ts = map gTreeHead (parsedPiece pr)
               printTreeHAn (pieceTreeHAn pr) (f1 ++ ".postProc") >> return ()
               printTreeHAnF ts f1 >> return () 
          -- Parse all files in one dir, show condensed output 
          (Nothing, Nothing, Just dir, False, Nothing, Nothing) ->
            parseDir g opts dir bOut
          -- ** audio ground-truth annotation part **   
          (Nothing, Just f1, Nothing, False, Just kf, Nothing) -> 
          -- parse a ground-truth annotation and its key and give verbose output      
            do key <- readFile kf
               pr  <- readFile f1 >>= parseAnnotationVerb g opts key
               print (pieceTreeHAn pr)
               mapM_ (print . gTreeHead) (take 10 $ parsedPiece pr)
          (Nothing, Just f1, Nothing, True , Just kf, Nothing) ->
          -- parse a ground-truth annotation and its key and print the parse 
            do key <- readFile kf
               pr  <- readFile f1 >>= parseAnnotation g opts key 
               let ts = map gTreeHead (parsedPiece pr)
               printTreeHAn (pieceTreeHAn pr) (f1 ++ ".postProc") >> return ()
               printTreeHAnF ts f1 >> return ()   
          -- Parse all files in one dir, show condensed output 
          (Nothing, Nothing, Just dir, False, Nothing, Just kd) ->
            parseAnnotationDir g opts kd dir
          -- Else throw error
          _ -> usageError args err1

--------------------------------------------------------------------------------
-- Match mode
--------------------------------------------------------------------------------
matchMode :: Mode [MyArgs]
matchMode = mode "match"  [Arg "Mode" "Match"] "Harmonic similarity matching"
              (flagArg upd0 "")
              ([flagReq ["m", "mode"]   upd1 "string" "Matching mode (stdiff|lces-s|lcessim|hanlign|align)"
               ,flagReq ["1", "sfile"]  (upd "SourceInputFile") "filepath" "Source file to match"
               ,flagReq ["2", "tfile"]  (upd "TargetInputFile") "filepath" "Target file to match"
               ,flagReq ["d", "dir"]    (upd "InputDir") "filepath" "Input directory to parse all files within"
               ,flagReq ["i", "in"]     (upd "BinaryIn") "filepath" "Input binary file for matching"
               ,flagNone ["s", "print-insertions"] ((Switch "PrintIns"):) "Show inserted nodes"
               ,flagReq ["g", "grammar"] updG          "string"   "Grammar to use (jazz|pop)"
               ] ++ inputFlags : helpVerFlags)

  where upd0 "--print-insertions" args = Right $ Switch "PrintIns" : args
        upd0 "-s"                 args = Right $ Switch "PrintIns" : args
        upd0 s                    _    = Left  $ "Unknown argument: " ++ s

        upd1 "stdiff"  args = Right $ (MatchMode STDiff)   : args
        upd1 "lces-s"  args = Right $ (MatchMode LCESsize) : args
        upd1 "lcessim" args = Right $ (MatchMode LCESsim)  : args
        upd1 "hanlign" args = Right $ (MatchMode HAnAlign) : args
        upd1 "align"   args = Right $ (MatchMode Align)    : args
        upd1 s         _    = Left  $ "Unknown match mode: " ++ s

mainMatch :: [MyArgs] -> IO ()
mainMatch args =
 do let cStr  = getArg args "SourceInputString"
        sif   = getArg args "SourceInputFile"
        mf2   = getArg args "TargetInputFile"
        mdir  = getArg args "InputDir"
        bIn   = getArg args "BinaryIn"
        me    = getArg args "MaxErrorRate"
        mode  = getMode args
        prnt  = gotArg args "Print"
        opts  = if gotArg args "PrintIns"
                then delete RemoveInsertions defaultOpts else defaultOpts

    -- Handle help and version, if present
    handleHelpVer args matchMode
    
    case (getGram args) of
      Nothing -> putStrLn "Please supply a grammar to use" >> exitFailure
      Just (GrammarEx g) ->
        case (cStr,sif,mf2,mdir,prnt) of
          -- Parse source and target file, show full output
          (_, Just f1, Just f2, Nothing, _)   -> 
            do c1 <- readFile' f1
               c2 <- readFile' f2
               matchFiles opts mode prnt c1 c2 f1 f2
          (Just c, Just f1, Nothing, Nothing, True) ->
            matchFiles opts mode True c f1 (trimFilename c) (trimFilename f1)
          -- match all files in one dir, show condensed output
          (_,Nothing, Nothing, Just dir, False) -> dirMatch g opts bIn mode (fmap read me) dir 
          _                                     -> usageError args err2

matchFiles :: [PPOption] -> MatchMode -> Bool -> String -> String 
           -> FilePath -> FilePath -> IO ()
matchFiles o m prnt f1 f2 n1 n2 = 
  -- should move to HarmTrace.IO.Main
  let (ParseResult key1 toks1 _ ts1 _nr1 te1 pe1 _) 
         = postProc o $ string2Piece Jazz f1
      (ParseResult key2 toks2 _ ts2 _nr2 te2 pe2 _) 
         = postProc o $ string2Piece Jazz f2
  in
  do  if not $ null te1 then showErrors "tokenizer 1: " te1 else putStr ""
      if not $ null te2 then showErrors "tokenizer 2: " te2 else putStr ""
      if not $ null pe1 then showErrors "parser 1: " pe1 else putStr ""
      if not $ null pe2 then showErrors "parser 2: " pe2 else putStr ""
      case (m,prnt) of
        (STDiff,_)      -> print (diffChordsLen toks1 toks2)
        (Align  ,False) -> print (getAlignDist key1 key2 toks1 toks2)
        (Align  ,True ) -> do let (mat,v,t) = alignChordLab key1 key2 toks1 toks2 
                              pPrintV t; print mat ; print v
                              
        (HAnAlign,True ) -> do let (mat,v,t) = alignHAnChord ts1 ts2
                               pPrintV t; print mat ; print v
        -- quick and dirty LCES plotting (should move to HarmTrace.IO)
        (LCESsize,True)  -> 
           do printTreeHAn ts1 (n1 ++ ".postProc") >> return ()
              printTreeHAn ts2 (n2 ++ ".postProc") >> return ()
              printTreeHAnF (fst $ getLCES ts1 ts2) 
                     (  (take 10 $ takeFileName n1) ++ ".vs." 
                     ++ (take 10 $ takeFileName n2) ++ ".lces") >> return ()
        _                -> error "Unimplemented."

--------------------------------------------------------------------------------
-- Recognise mode
--------------------------------------------------------------------------------

recogniseMode :: Mode [MyArgs]
recogniseMode = mode "recognise" [Arg "Mode" "Recognise"] "Recognise chords from audio files"
              (flagArg upd0 "")
              ([flagReq ["m", "mode"]   upd1                           "string"   "Recognition mode (mptree|group|simple|mpt-stg)"
               ,flagReq ["i", "file"]   (upd "SourceInputFile")        "filepath" "Input file"
               ,flagReq ["l", "list"]   (upd "SourceInputFileList")    "filepath" "List with input files"
               ,flagReq ["c", "gt"]     (upd "GroundTruthInputFile")   "filepath" "Ground truth chord annotation file (for evaluation)"
               ,flagReq ["k", "key"]    (upd "SourceKeyInputFile")     "filepath" "Ground truth key annotation file"
               ,flagReq ["d", "dir"]    (upd "InputDir")               "filepath" "Input directory"
               ,flagReq ["v", "vamp-dir"] (upd "VampBaseDir")          "filepath" "Vamp-plugin transform specification base directory"
               ,flagReq ["o", "out-dir"]  (upd "OutputDir")            "filepath" "output directory (for logs and chords)"
               ,flagReq ["f", "csv-dir"]  (upd "CSVBaseDir")           "filepath" "directory for storing the feature csv files"
               ,flagReq ["w", "log-dir"]  (upd "LogBaseDir")           "filepath" "directory for storing the feature extraction logs (for every processed file)"
               ,flagReq ["s", "sa-path"]  (upd "SonicAnnotator")       "filepath" "Path to the sonic annotator executable (it is assumed to be in the path by default)"
               ,flagReq ["t", "gt-dir"] (upd "GroundTruthInputDir")    "filepath" "Ground-truth chord annotation input directory (for evaluation)"
               ,flagReq ["x", "key-dir"] (upd "AnnotationKeyInputDir") "filepath" 
   ("Ground-truth key annotation input directory (for evaluation). " ++
   "This flag also functions as a switch for selecting harmtrace chord " ++
   "recognision using ground-truth key annotations. Using this flag without " ++
   "the --gt-dir flag will trigger the evaluation of the key finding only. " ++
   "Similarly, using this flag together with -i will trigger the key finding" ++
   "of that specific file")
               ,flagReq ["g", "grammar"] updG                          "string"   "Grammar to use (jazz|pop)"
               ,flagNone ["p", "print"] ((Switch "Print"):)                       "Output a .png of the parse tree"
               ] ++ inputFlags : helpVerFlags)

  where upd0 "--print"            args = Right $ Switch "Print"    : args
        upd0 "-p"                 args = Right $ Switch "Print"    : args
        upd0 s                    _    = Left  $ "Unknown argument: " ++ s

        upd1 "group"  args = Right $ (RecognitionMode (RecognitionFun groupAnnotator))  : args
        upd1 "simple" args = Right $ (RecognitionMode (RecognitionFun simpleAnnotator)) : args
        upd1 "mptree" args = Right $ (RecognitionMode (RecognitionFun mptreeAnnotator)) : args
        upd1 "mpt-stg" args = Right $ (RecognitionMode (RecognitionFun mptreeAnnotatorSTG)) : args
        upd1 s        _    = Left  $ "Unknown recognition mode: " ++ s

mainRecognise :: [MyArgs] -> IO ()
mainRecognise args = 
 do let af     = getArg args "SourceInputFile"
        gt     = getArg args "GroundTruthInputFile"
        lst    = getArg args "SourceInputFileList"
        key    = getArg args "SourceKeyInputFile"
        afdir  = getArg args "InputDir"
        gtdir  = getArg args "GroundTruthInputDir"
        keydir = getArg args "AnnotationKeyInputDir"
        prnt   = gotArg args "Print"
        mout   = getArg args "OutputDir"
        dirs   = setPaths (getArg args "VampBaseDir")
                          (getArg args "CSVBaseDir")
                          mout
                          (getArg args "LogBaseDir")
                          (getArg args "SonicAnnotator")
    
    -- Handle help and version, if present
    handleHelpVer args recogniseMode

    case (getGram args) of
      Nothing -> putStrLn "Please supply a grammar to use" >> exitFailure
      Just g ->
        let ann :: Maybe [TimedData Key] -> AudioFeat -> ChordAnnotation
            ann = (getAnn args) g
        in case (gt,af,key,gtdir,afdir,keydir,lst) of
          -- evaluates a single audio feature set
          (Just g, Just a, Nothing, Nothing, Nothing, Nothing, Nothing) -> 
            evaluateLabeling ann prnt g a Nothing >>= print 
          -- evaluates a single audio feature set and key annotation
          (Just g, Just a, maybek, Nothing, Nothing, Nothing, Nothing) -> 
            evaluateLabeling ann prnt g a maybek >>= print
          (Nothing, Just a, Nothing, Nothing, Nothing, Just keydir, Nothing) -> 
            evaluateKeyFinding prnt a keydir  
          -- evaluates a single audio feature set and prints the auto-labelling
          -- This is the only option that automatically extracts features
          -- from an audio file (if no features, and an audio file are found)
          -- TODO enable feature extraction for all options
          (Nothing, Just a, Nothing, Nothing, Nothing, Nothing, Nothing) -> 
            printLabelling defaultChordPrinting dirs ann a   
          -- takes a list of files and prints the chords to files
          (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just a) -> 
            batchPrintLabelling defaultChordPrinting dirs ann a   
          -- evaluates a directory with audio features, when an output flag
          -- is present we also store the chords in a file (the actual 
          -- output dir is stored in a 'BasePaths')
          (Nothing, Nothing, Nothing, Just gd, Just ad, Nothing, Nothing) -> 
            batchLabeling defaultChordPrinting mout ann gd ad Nothing
          -- evaluates a directory with audio features and key annotations
          (Nothing, Nothing, Nothing, Just gd, Just ad, maybek, Nothing) -> 
            batchLabeling defaultChordPrinting mout ann gd ad maybek
          -- if we only encounter audio features and key annotations (and no
          -- ground-truth chords), we evaluate only the key finding. I agree
          -- this is rather hacky... (the annotator options are completely 
          -- ignored)
          (Nothing, Nothing, Nothing, Nothing, Just ad, Just key, Nothing) -> 
            batchEvaluateKeyFinding ad key
          _                                        -> usageError args err4

--------------------------------------------------------------------------------
-- Arguments and utilities
--------------------------------------------------------------------------------
data MyArgs = Grammar GrammarEx
            | MatchMode MatchMode
            | RecognitionMode RecognitionFun
            | Arg String String
            | Switch String
            deriving (Eq, Show)

newtype RecognitionFun = RecognitionFun (GrammarEx -> Maybe [TimedData Key]
                                           -> AudioFeat -> ChordAnnotation)

instance Show RecognitionFun where
  show _ = "RecognitionFun"

instance Eq RecognitionFun where
  _ == _ = error "please do not use this"

-- Grammar parser
updG :: String -> [MyArgs] -> Either String [MyArgs]
updG "jazz" args = Right $ (Grammar (GrammarEx Jazz)) : args
updG "pop"  args = Right $ (Grammar (GrammarEx Pop))  : args
updG s      _    = Left  $ "Unknown grammar: " ++ s

-- General update function
upd :: String -> String -> [MyArgs] -> Either String [MyArgs]
upd c s args = Right $ (Arg c s) : args

getGram :: [MyArgs] -> Maybe GrammarEx
getGram []              = Nothing
getGram ((Grammar g):_) = Just g
getGram (_:t)           = getGram t

getMode :: [MyArgs] -> MatchMode
getMode []                = error "getMode impossible?"
getMode ((MatchMode m):_) = m
getMode (_:t)             = getMode t

getAnn :: [MyArgs] -> (GrammarEx -> Maybe [TimedData Key] -> AudioFeat
                       -> ChordAnnotation)
getAnn []                                       = error "getAnn impossible?"
getAnn ((RecognitionMode (RecognitionFun f)):_) = f
getAnn (_:t)                                    = getAnn t

gotArg :: [MyArgs] -> String -> Bool
gotArg []             _          = False
gotArg ((Switch k):_) s | k == s = True
gotArg (_:t)          s          = gotArg t s 

getArg :: [MyArgs] -> String -> Maybe String
getArg []            _             = Nothing
getArg ((Arg k v):_) s | k == s    = Just v
getArg (_:t)         s             = getArg t s

-- Help and Version flags, used in all modes
helpVerFlags :: [Flag [MyArgs]]
helpVerFlags = [ flagHelpSimple ((Switch "Help"):)
               , flagVersion ((Switch "Version"):)]

-- Handler for the Help and Version flags
handleHelpVer :: [MyArgs] -> Mode [MyArgs] -> IO ()
handleHelpVer args mode = do
    let ver  = gotArg args "Version"
        help = gotArg args "Help"

    -- Output version and exit
    when ver $ putStrLn vERSION >> exitSuccess

    -- Output help and exit
    when help $
      print (helpText [] HelpFormatDefault mode) >> exitSuccess

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------
err1, err2, err3, err4 :: String
err1 = "Use a source file, or a directory."
err2 = "Use a source file and a target file, or a directory."
err3 = "Use a source file and optionally a target file."
err4 = "Use an audio-feature location and a ground-truth file, "++
       "or an audio-feature directory and a ground-truth directory."

usageError :: [MyArgs] -> String -> IO ()
usageError _args err = putStrLn err

trimFilename :: String -> String
trimFilename = filter (\x -> not (elem x ":*")) . concat . words . take 20 

-- by default all post processing operations are executed             
defaultOpts :: [PPOption]
defaultOpts  = [ RemovePDPT    , RemoveInsertions
               , MergeDelChords, ExpandChordDurations ]

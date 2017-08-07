{-# OPTIONS_GHC -Wall         #-}
{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.IO.FeatExtract
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: A module that wraps arround the sonic annotator 
-- (<http://omras2.org/SonicAnnotator>), which is used to extract spectral
-- and pulse features from an audio source.
--------------------------------------------------------------------------------

module HarmTrace.IO.FeatExtract (extractFeatures) where

import System.IO
import System.FilePath 
import System.Directory (removeFile)
import System.Process 
import System.Exit (ExitCode (..))
import Data.List (isPrefixOf)

import Constants ( keyStr, chromaStr, beatStr, deleteDownsampledWav, downSample)
import HarmTrace.Base.Parsing
import HarmTrace.IO.Downsample
import HarmTrace.IO.BasePaths( BasePaths, getBeatVampPath, basePathsExist
                             , getKeyVampPath, getChromaVampPath
                             , getFeatDir, getSonic, getLogFile)


-- | Extracts the features of an arbitrary audio file, given three VAMP plugin
-- descriptions. Before the feature extraction the audio is preprocessed 
-- (downsampled) with the the SoX library (see 'HarmTrace.IO.Downsample')
-- See for more information:
--
-- * <http://www.vamp-plugins.org>
--
-- * <http://omras2.org/SonicAnnotator>
extractFeatures :: BasePaths -> FilePath 
                -> IO (Maybe (FilePath,FilePath,FilePath))
extractFeatures dirs f = do
  -- check if all files and directories exists, if not fail.
  exists <- basePathsExist dirs
  case exists of 
    False ->  return Nothing
    True  ->  -- create a log file
             do let -- logf = getOutDir  dirs </> takeFileName f <.> "extract.log"
                    cfp  = getFeatDir dirs </> 
                           dropExtension (takeFileName f) ++ chromaStr
                    bfp  = getFeatDir dirs </> 
                           dropExtension (takeFileName f) ++ beatStr
                    kfp  = getFeatDir dirs </> 
                           dropExtension (takeFileName f) ++ keyStr
                
                -- open a logfile
                hdl <- openFile (getLogFile dirs f) AppendMode
                hSetBuffering hdl LineBuffering
                
                -- downsample the audio
                hPutStrLn hdl (f ++ ";preprocessing;0;0")
                df <- case downSample of 
                        True  -> downsample f
                        False -> return f
                hPutStrLn hdl (f ++ ";preprocessing;99;9")
              
                -- extract the features
                sonicannotatorProcPipe dirs (LogInfo f "" 0 10) hdl 
                                       (getBeatVampPath dirs)   df bfp
                sonicannotatorProcPipe dirs (LogInfo f "" 0 30) hdl 
                                       (getChromaVampPath dirs) df cfp
                sonicannotatorProcPipe dirs (LogInfo f "" 0 50) hdl 
                                       (getKeyVampPath dirs)    df kfp
                hClose hdl
                
                -- remove downsampled wav file?
                when deleteDownsampledWav $ void (removeFile df)
                -- when (maximum (exitBeat : exitChrm : [exitKey]) > 0) 
                return (Just (cfp,bfp,kfp))

-- executes the sonicannotator and logs all execution info to a log file  
-- TODO: perhaps output a Bool for loggin if the annotating is successful
sonicannotatorProcPipe :: BasePaths -> LogInfo -> Handle -> FilePath 
                       -> FilePath -> FilePath -> IO ()
sonicannotatorProcPipe bp info logh transformfile inp outp= do
  let par = "--transform" : transformfile : "--writer" : "csv" 
          : "--csv-one-file" : [outp, inp]
  (_,_,Just err,p) <- createProcess (proc (getSonic bp) par) 
                                    {std_err = CreatePipe}
      --                            {std_out = CreatePipe}                                    
  cerr <- hGetContents err
  --  cout <- hGetContents out
  --  print cerr 
  --  print cout 
  let tlog = transformLog info cerr
  hPutStr logh $ concatMap show tlog
  void (waitForProcess p)

  (Just ex) <- getProcessExitCode p 
  -- print the error when the sonic-annotator fails
  when (ex /= ExitSuccess) 
       (putStr ("sonic annotator failed to exit normally:\n" ++ cerr))
  
--------------------------------------------------------------------------------  
-- parsing sonic annotator logs
--------------------------------------------------------------------------------
  
-- transforms a sonic annotator log file into a file that should be easy to
-- parse, satisfying the following format: file;feature_id;percentage;total_perc
transformLog :: LogInfo -> String -> [LogInfo] -- String
transformLog a = combineLine a . map (parseData pLogLine) . filter f . rLines 
  where
  -- a filter that selects the lines that contain the information we want
  f :: String -> Bool
  f x = isPrefixOf "Extracting" x || isPrefixOf "    id=" x
  -- a variant of lines that also splits on a '\r'
  rLines :: String -> [String]
  rLines s = let (l, s') = break (\x -> ((x == '\n') || (x == '\r'))) s
             in  l : case s' of
                          []      -> []
                          (_:s'') -> rLines s''
  
  -- combines LogLines into LogInfo datatypes that collects the data in the 
  -- individual lines
  combineLine :: LogInfo -> [LogLine] -> [LogInfo]
  combineLine i []     = [i]
  combineLine i (l:ls) = let i' = comb i l in i' : combineLine i' ls 
  -- comb does the actual combining
  comb :: LogInfo -> LogLine -> LogInfo
  -- we do not updated the filename because this file will contain a
  -- "downsampled.wav" suffix, and we want the orginal file to appear in the log
  comb info (LogFile _) = info -- {file    = n} 
  comb info (LogFeat d) = info {feature = d}
  comb info (LogPerc p) = let t = if p `mod` 5 == 0 then 1 else 0 
                          in  info {perc = p, total = total info + t}                 
                          
-- a datatype storing for collecting the information in the log file
data LogInfo = LogInfo {_file   :: String,
                        feature :: String,
                        perc    :: Int,
                        total   :: Int}
instance Show LogInfo where
  show (LogInfo f d p t) 
    | null f    = ""
    | otherwise = f ++ ';' : d ++ ';' : show p ++ ';' : show t ++ "\n"

-- a datatype for storing the information in one line of the log file  
data LogLine = LogFile String
             | LogFeat String
             | LogPerc Int deriving Show
    
-- the actual parsers of the log file
pLogLine :: P (Str Char String LineColPos) LogLine
pLogLine =   LogFile <$> pFileName
         <|> LogFeat <$> pFeatName
         <|> LogPerc <$> pPerc

-- the percentage of the currently extracted feature
pPerc :: P (Str Char String LineColPos) Int
pPerc =     pString "Extracting and writing features... " 
        *> (pInteger <* pSym '%' <|> f <$> pString "Done")
        <*  pList pAscii -- if there is anything left 
  where f x = if x == "Done" then 100 else -1

-- the filename
pFileName :: P (Str Char String LineColPos) String
pFileName =  pString "Extracting features for: " *> pQuotedString 

-- the feature description
pFeatName :: P (Str Char String LineColPos) String
pFeatName =  pString "    id=" *> pQuotedString 

-- test :: IO ()
-- test =
  -- do t <- readFile "D:\\chordify\\harmtrace\\test.txt"
     -- mapM_ print (transformLog (LogInfo "" "" 0 20) t)         

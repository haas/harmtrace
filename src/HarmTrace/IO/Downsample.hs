module HarmTrace.IO.Downsample (downsample) where

import Constants (outputSampleRate, outputBitsPerSample, outputNrOfChannels)

import Sound.Sox.Convert (simple)
import Sound.Sox.Option.Format

import Data.Monoid
import System.Exit (ExitCode (..))

-- import System.Directory
-- import System.FilePath 

-- ** N.B. SoX is required **
-- http://sox.sourceforge.net/
-- install on Linux:   apt-get install sox libsox-fmt-all (all file formats)
-- install on Windows: you can download the binary from the sox website. 
--                     However, this binary does not have .mp3 support. This
--                     website provides a binary that does:
-- http://www.codeproject.com/Articles/33901/Compiling-SOX-with-Lame-and-Libmad-for-Windows

downsample :: FilePath -> IO (FilePath)
downsample fp = 
  do let o   = mconcat [ numberOfChannels outputNrOfChannels
                       , bitsPerSample    outputBitsPerSample
                       , sampleRate       outputSampleRate   ]
         out = fp ++ ".downsampled.wav"
         -- out = takeDirectory fp </> "downsampled" </> takeFileName fp
     conv <- simple none fp o out
     if   conv /= ExitSuccess 
          then error "HarmTrace.IO.Downsample: sox did not terminate normally"
          else return out

-- given a path to a directory with wav files (anything other than wav files
-- is ignored will be downsamnpled.          
-- batchDownsample :: FilePath -> IO ()
-- batchDownsample fp = 
  -- do fs <- getDirectoryContents fp
     -- let fs' = filter ((==".wav") . takeExtension) (map (\x -> fp </> x) fs)
     -- mapM_ downsample fs'

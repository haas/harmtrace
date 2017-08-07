module HarmTrace.IO.BasePaths ( BasePaths (..), setPaths, getLogFile
                              , consBasePaths, basePathsExist) where

import System.FilePath ((</>), (<.>), takeFileName)
import HarmTrace.IO.Common (fileExists, dirExists)
import Constants ( defaultVampDir, defaultOutDir, defaultFeatDir, defaultLogDir
                 , beatVampPath, chromaVampPath, keyVampPath
                 , sonicAnnotator, logFileSuffix)

-- | A datatype for storing the base directories of the vamp plugins, feature
-- csv files, and the chord and log Files.
data BasePaths = BasePaths { getVampDir :: FilePath
                           , getFeatDir :: FilePath
                           , getOutDir  :: FilePath
                           , getLogDir  :: FilePath
                           , getSonic   :: FilePath
                           , getBeatVampPath   :: FilePath
                           , getChromaVampPath :: FilePath
                           , getKeyVampPath    :: FilePath
                           }

-- | Similar to 'consBasePaths', but instead excepts 'Maybe FilePath's instead
-- of 'FilePath's
setPaths :: Maybe FilePath -> Maybe FilePath -> Maybe FilePath
         -> Maybe FilePath -> Maybe FilePath -> BasePaths
setPaths mv mf mo ml msa = 
  let vamp = maybe defaultVampDir id mv 
      feat = maybe defaultFeatDir id mf 
      out  = maybe defaultOutDir id mo
      logd = maybe defaultLogDir id ml
      sa   = maybe sonicAnnotator id msa -- there is a specific function without id, but hackage is down
  in consBasePaths vamp feat out logd sa

-- | contructs a new 'BasePaths' based on a VAMP transform directory, 
-- a feature directory (to store the CSV files), and an output directory.
consBasePaths :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath 
              -> BasePaths
consBasePaths vamp feat out logd sa = BasePaths vamp feat out logd sa
  (vamp </> beatVampPath) (vamp </> chromaVampPath) (vamp </> keyVampPath) 

-- | checks (verbosely) if the paths stored in a 'BaseDir' exist.  
basePathsExist :: BasePaths -> IO (Bool)
basePathsExist dirs = do v <- fileExists (getBeatVampPath dirs)
                         c <- fileExists (getChromaVampPath dirs)  
                         k <- fileExists (getKeyVampPath dirs)
                         -- l <- fileExists (getLogDir dirs)
                         f <- dirExists  (getFeatDir dirs)
                         o <- dirExists  (getOutDir dirs)
                         return (v && c && k && f && o)
                         
getLogFile :: BasePaths -> FilePath -> FilePath
getLogFile bp fp = getLogDir bp </> takeFileName fp <.> logFileSuffix
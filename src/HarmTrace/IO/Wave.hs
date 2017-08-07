
--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.IO.Wave
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Read .wav files into hmatrix.
--------------------------------------------------------------------------------

module HarmTrace.IO.Wave (

  readWave, waveToVector

  ) where


-- HCodecs
import Codec.Wav            ( importFile )
import Data.Audio           ( Audio, sampleData, convert )

-- hmatrix
import Data.Packed.Vector   ( Vector, fromList )

-- Other
import Data.Array.Unboxed   ( elems )
import Data.Int             ( Int32 )

--------------------------------------------------------------------------------
-- Reading wave files
-------------------------------------------------------------------------------- 

-- | Reads a wave file from disk into a vector
readWave :: FilePath -> IO (Vector Double)
readWave fp = do r <- importFile fp
                 case r of
                   Left err -> putStrLn ("Error reading .wav file: " ++ err)
                                >> return (fromList []) -- return empty Vector
                   Right a  -> return (waveToVector a)

-- | Converts HCodecs' internal Audio representation to an hmatrix Vector.
-- Innefficient at the moment, because of list conversions.
waveToVector :: Audio Int32 -> Vector Double
waveToVector = fromList . elems . convert . sampleData

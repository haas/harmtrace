
--------------------------------------------------------------------------------
-- |
-- Module      :  Replace
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Script that fills out the 'vERSION' in 'Constants'
--------------------------------------------------------------------------------

module Replace where

import System.IO
import System.Process (readProcess)
import Data.List (isPrefixOf)
import HarmTrace.IO.Common (hGetContents')


main :: IO ()
main = do handle <- openFile "Constants.hs" ReadMode
          file   <- hGetContents' handle
          hClose handle

          commit <- readProcess "git" ["describe", "--tags"] ""
                 -- readProcess "git" ["rev-parse", "HEAD"] ""

          let needle = "gitVersion = "
              -- The `init` is to remove the \n at the end
              newver = needle ++ "\"" ++ init commit ++ "\""
              perLine line = if needle `isPrefixOf` line then newver else line

          handle <- openFile "Constants.hs" WriteMode
          hPutStr handle (unlines (map perLine (lines file)))
          hClose handle

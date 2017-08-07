module Main (main) where

import Distribution.Simple
import System.Cmd (system)

main :: IO ()
main = defaultMainWithHooks hooks
  where hooks = simpleUserHooks { runTests = runTests'
                                , preBuild = preBuild' }

-- Runs the testsuite
runTests' _ _ _ _ = system "shelltest tests -c -d -- --threads=4" >> return ()

-- Update the VERSION string before building
--preBuild' :: Args -> BuildFlags -> IO HookedBuildInfo
preBuild' a b = do -- First do our replacement
                   system "cd src && runghc Replace.hs"
                   -- Then go on with preBuild as usual
                   preBuild simpleUserHooks a b
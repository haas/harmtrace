{-# OPTIONS_GHC -Wall         #-}
module HarmTrace.IO.Errors where

-- Parser stuff
import Text.ParserCombinators.UU.BasicInstances  as PC (Error (..))

import Data.List (genericLength)
import System.IO (stderr, hPutStrLn)
--------------------------------------------------------------------------------
-- Error Reporting
--------------------------------------------------------------------------------

data ErrorNrs = ErrorNrs { ins :: Int, del :: Int, delEnd :: Int, rep :: Int }

-- datatype for storing the number of different error types
instance Show ErrorNrs where 
   show (ErrorNrs i d e r) = show i ++ " insertions, " ++ show d 
        ++ " deletions, " ++ show r ++ "replacements, and " 
        ++ show e ++ " unconsumed tokens"

-- More concise showing errors, and in IO
showErrors :: Show a => String -> [Error a] -> IO ()
showErrors label l = case countErrors l of
  ErrorNrs i d e r -> hPutStrLn stderr (label ++ show i ++ " insertions, " 
                                      ++ show d ++ " deletions, "
                                      ++ show r ++ " replacements, "
                                      ++ show e ++ " deletions at the end")
-- Counts the number of insertions and deletions
countErrors :: Show a => [Error a] -> ErrorNrs
countErrors [] = ErrorNrs 0 0 0 0
countErrors ((PC.Inserted _ _ _)  :t) = inc1 (countErrors t)
countErrors ((PC.Deleted _ _ _)   :t) = inc2 (countErrors t)
countErrors ((DeletedAtEnd _)  :t)    = inc3 (countErrors t)
countErrors ((Replaced _ _ _ _):t)    = inc4 (countErrors t)

simpleErrorMeasure :: ErrorNrs -> Float
simpleErrorMeasure (ErrorNrs i d e r) = fromIntegral (i + d + e + r)

errorRatio :: Show a => [Error a] -> [b] -> Float
errorRatio errs toks = simpleErrorMeasure (countErrors errs) /
   -- probably we should not divide here by "mergeDups" ...
   -- genericLength (mergeDups (Key (Note Nothing C) MajMode) toks)
   genericLength toks

inc1, inc2, inc3, inc4 :: ErrorNrs -> ErrorNrs
inc1 e = e { ins    = ins e    + 1 }
inc2 e = e { del    = del e    + 1 }
inc3 e = e { delEnd = delEnd e + 1 }
inc4 e = e { rep    = rep e    + 1 }

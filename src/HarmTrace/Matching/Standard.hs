
module HarmTrace.Matching.Standard (diffChords, diffChordsLen) where

import Data.Algorithm.Diff -- cabal install Diff

diff :: (Eq a) => [a] -> [a] -> [(DI,a)]
diff = getDiff

diffLen :: (Eq a) => [a] -> [a] -> Float
diffLen x y = fromIntegral (len (diff x y)) / fromIntegral (length x)

len :: [(DI,a)] -> Int
len []        = 0
len ((B,_):t) = len t
len ((_,_):t) = 1 + len t

--------------------------------------------------------------------------------
-- Matching
--------------------------------------------------------------------------------

diffChordsLen :: (Eq a) => [a] -> [a] -> Float
diffChordsLen = diffLen

diffChords :: (Show a, Eq a) => [a] -> [a] -> String
diffChords x y = show (diff x y)

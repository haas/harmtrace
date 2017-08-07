
--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Audio.Statistical
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Some statistical functions used in processing audio.
--------------------------------------------------------------------------------
module HarmTrace.Audio.Statistical where

import HarmTrace.Base.MusicTime

import Data.List ( genericLength, tails
                 , inits,maximumBy, sort, group)
import Data.Ord (comparing)

--------------------------------------------------------------------------------
-- Statistical functions
--------------------------------------------------------------------------------

-- | Similar to group, but in case a group is smaller than s, 
-- the group is filled with the same number of a's:
-- 
-- >>> groupMinSize 3 0 [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5]
-- [[0],[0,0],[0,0,0],[4,4,4,4],[5,5,5,5,5]]
groupMinSize :: Eq a => Int -> a -> [a] -> [[a]]
groupMinSize _ _  []    =  []
groupMinSize s a (x:xs) =  grp : groupMinSize s a zs
   where (ys,zs) = span (== x) xs
         lys = length ys
         grp = if length ys >= s then x:ys else replicate (lys + 1) a    

-- | Returns the mean of list.
listMean :: [NumData] -> NumData
listMean a = sum a  / genericLength a
  
-- | a median filter: see <http://en.wikipedia.org/wiki/Median_filter>.
medianFilter :: Ord a => Int -> [a] -> [a]
medianFilter wsize l = map mode $ getWindows wsize l
  
-- | Returns a list with all "sliding windows" of a particular size.
-- The left and right edge of the list are filled with the first and last (size 
-- /2) items, respectively and the remainder is filled with the mode/median of
-- the complete list.
getWindows :: Ord a => Int -> [a] -> [[a]]
getWindows size l = lbor ++ mid ++ rbor
  where mid  = takeWhile (hasSize size) . map (take size) $ tails l
        gmed = mode l
        ls   = size `div` 2
        rs   = size - ls - 1
        lbor = reverse $ map (fillWith size gmed) 
                             (reverse . dropWhile null . inits $ take ls l)
        rbor = map (reverse . fillWith size gmed) 
                   (takeWhile (not . null) . tails . take rs $ reverse l)
        -- in general (length l) < x, but this is guaranteed within this let
        fillWith ::  Int -> a -> [a] -> [a]
        fillWith x a lt = replicate (x - length l) a ++ lt

hasSize :: Int -> [a] -> Bool
hasSize s l = length l >= s
  
-- | The mode: the element that occurs most often in the collection.
mode   :: Ord a => [a] -> a
mode [] = error "Key.hs: mode called on []"
mode l  = head . maximumBy (comparing length) . group $ sort l 
  
-- | Returns the median of a list.
median   :: Ord a => [a] -> a -- the median not good for keys... 
median [] = error "Key.hs: median called on []"
median l  = sort l !! (length l `div` 2)  

-- | Returns the index of th maximal element of a list.
maxListIndex :: Ord a => [a] -> Int
maxListIndex = fst . maxPair where
  maxPair :: Ord a => [a] -> (Int,a)
  maxPair = maximumBy (comparing snd) . zip [0..] 



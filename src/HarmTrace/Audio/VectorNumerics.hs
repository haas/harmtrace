{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module HarmTrace.Audio.VectorNumerics ( -- Matrix
                                       -- , Vector
                                       -- , ix
                                        --  disp
                                      --  , dispf
                                       -- | * Numerical calculation
                                         scale
                                       , sum
                                       , mean
                                       , norm2
                                       , Num ()
                                       -- | * Utilities
                                       -- | * List converstions
                                      --  , fromLists
                                      --  , toLists
                                       , V.fromList
                                       ) where

import qualified Data.Vector as V
import Data.Vector ( Vector )
-- import Data.List   ( intercalate )
-- import Text.Printf ( printf, PrintfArg )
import Prelude hiding (sum)
{-
-- | A matrix is a Vector of Vectors, we could also use one large Vector with
-- another 'ix' function
type Matrix a = Vector (Vector a)

-- | Displaying a matrix
disp :: Show a => Matrix a -> String
disp = disp' dispRow

  where dispRow :: Show a => Vector a -> String
        dispRow = intercalate " " . V.foldr (\j js -> show j : js ) []

-- | Displaying a matrix
dispf :: PrintfArg a => Matrix a -> String
dispf = disp' dispRow

  where dispRow :: PrintfArg a => Vector a -> String
        dispRow = concat . V.foldr (\j js -> printf "\t%.2f" j : js ) []

-- | Displaying a matrix
disp :: (Vector a -> String) -> Matrix a -> String
disp dispRow  = intercalate "\n" . V.foldr (\i is -> dispRow i : is) ["\n"]
-}
-- | replaces a 0.0 by a very small number 1.0e-10
-- replaceZero :: Vector Prob -> Vector Prob
-- replaceZero = V.map repl where

  -- repl :: Prob -> Prob
  -- repl 0.0 = 1.0e-10
  -- repl p   = p

--------------------------------------------------------------------------------
-- Numerical Vectors
--------------------------------------------------------------------------------

-- using a Matrix
instance Num a => Num (Vector a) where
  va + vb = mergeVectorsSameSize (+) va vb
  va - vb = mergeVectorsSameSize (-) va vb
  va * vb = mergeVectorsSameSize (*) va vb
  signum  = V.map signum
  negate  = V.map negate
  abs     = V.map abs
  fromInteger = V.singleton . fromInteger

-- | Scales a numerical Vector
scale :: Num a => a -> Vector a -> Vector a
scale s = V.map (s *)

-- | Sums the elements of the Vector
sum :: Num a => Vector a -> a
sum = V.foldr (+) 0

-- | Calculates the mean of the elements of the Vectors
mean :: (Fractional a, Num a) => Vector a -> a
mean v = sum v / fromIntegral (V.length v)

-- merges to Vectors by applying a function to index pairs of both Vectors
mergeVectorsSameSize :: Num a => (a -> a -> a)
                     -> Vector a -> Vector a -> Vector a
mergeVectorsSameSize f va vb
  | V.length va == V.length vb = V.zipWith f va vb
  | otherwise = error (  "mergeVectorsSameSize: vectors of different sizes: "
                      ++ show (V.length va) ++ " and " ++ show (V.length vb))

norm2 :: (Floating a, Num a) => Vector a -> a
norm2 v = sqrt $ sum (v * v)

--------------------------------------------------------------------------------
-- Numerical Vectors
--------------------------------------------------------------------------------
{-
fromLists :: [[a]] -> Matrix a
fromLists = V.fromList . map V.fromList

toLists :: Matrix a -> [[a]]
toLists = V.toList . V.map V.toList
-}

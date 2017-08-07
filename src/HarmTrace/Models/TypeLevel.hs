{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE DeriveDataTypeable       #-}

module HarmTrace.Models.TypeLevel (
      Su, Ze 
    , T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10
    , T11, T12, T13, T14, T15, T16, T17, T18, T19, T20
    , ToNat(..)
  ) where

import Data.Typeable


-- Type level peano naturals
data Su :: * -> *  deriving Typeable
data Ze :: *       deriving Typeable

-- Some shorthands
type T0 = Ze
type T1 = Su T0
type T2 = Su T1
type T3 = Su T2
type T4 = Su T3
type T5 = Su T4
type T6 = Su T5
type T7 = Su T6
type T8 = Su T7
type T9 = Su T8
type T10 = Su T9
type T11 = Su T10
type T12 = Su T11
type T13 = Su T12
type T14 = Su T13
type T15 = Su T14
type T16 = Su T15
type T17 = Su T16
type T18 = Su T17
type T19 = Su T18
type T20 = Su T19

class ToNat n where
  toNat :: n -> Int

instance ToNat Ze where toNat _ = 0
instance (ToNat n) => ToNat (Su n) where toNat _ = 1 + toNat (undefined :: n)
{-
-- Below is some experimentation...

-- A degree has a distance to root in semi-tones (n in T0..T11) and a 
-- class (major or minor)
data Degree n cls

-- Transposing is a bit like addition...
type family Transpose m n
-- ... but we normalize at the end to stay within T0..T11
type instance Transpose m T0 = Norm m
type instance Transpose m (Su n) = Transpose (Su m) n

-- Normalizing is the same as subtracting T12, but only if we can. Else we keep
-- the type unchanged.
type Norm m = Sub m T12 m

-- Subtraction with an extra type for failure
type family Sub m n fail
-- Inductive case
type instance Sub (Su m) (Su n) fail = Sub m n fail
-- Base case, subtraction succeeded
type instance Sub m T0 fail = m
-- Base case, subtraction failed
type instance Sub T0 (Su n) fail = fail

-- A secondary dominant is a transposition by 7 semi-tones
type SD deg = Transpose deg T7

-- A tritone substitution is a transposition by 6 semi-tones
type TS deg = Transpose deg T6
-}

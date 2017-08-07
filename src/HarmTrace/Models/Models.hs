{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE KindSignatures               #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Models.Models
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: All the models
--------------------------------------------------------------------------------

module HarmTrace.Models.Models where

import HarmTrace.Models.Jazz.Model as J hiding (PD, PT) 
import HarmTrace.Models.Pop.Model  as P hiding (PD, PT)

import HarmTrace.HAnTree.ToHAnTree


data Grammar :: * -> * where
  Jazz :: Grammar J.Piece
  Pop  :: Grammar P.Piece

data GrammarEx where
  GrammarEx :: (GTree g) => Grammar g -> GrammarEx

instance Show GrammarEx where
  show (GrammarEx Jazz) = "JazzGrammar"
  show (GrammarEx Pop ) = "PopGrammar"

instance Eq GrammarEx where
  (GrammarEx Jazz) == (GrammarEx Jazz) = True
  (GrammarEx Pop ) == (GrammarEx Pop ) = True
  _                == _                = False

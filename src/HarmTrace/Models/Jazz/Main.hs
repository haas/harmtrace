
--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Models.Jazz.Main
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: The jazz parser
--------------------------------------------------------------------------------

module HarmTrace.Models.Jazz.Main ( 
    pJazz 
  , module HarmTrace.Models.Jazz.Model
  ) where

-- Parser stuff
import Text.ParserCombinators.UU

-- Music stuff
import HarmTrace.Base.MusicRep
import HarmTrace.Models.Parser
import HarmTrace.Models.Jazz.Model hiding (PD,PT)

import HarmTrace.Models.Jazz.Instances ()


--------------------------------------------------------------------------------
-- From tokens to structured music pieces
--------------------------------------------------------------------------------

pPieceMaj, pPieceMin :: PMusic [Piece]
pPieceMaj = map Piece <$> amb (parseG :: PMusic [Phrase MajMode])
pPieceMin = map Piece <$> amb (parseG :: PMusic [Phrase MinMode])

pJazz :: Key -> PMusic [Piece]
pJazz (Key _ MajMode) = pPieceMaj
pJazz (Key _ MinMode) = pPieceMin

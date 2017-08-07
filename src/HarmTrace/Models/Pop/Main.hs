
module HarmTrace.Models.Pop.Main ( 
    pPop
  , module HarmTrace.Models.Pop.Model
  ) where

-- Parser stuff
import Text.ParserCombinators.UU

-- Music stuff
import HarmTrace.Base.MusicRep
import HarmTrace.Models.Parser
import HarmTrace.Models.Pop.Model hiding (PD,PT)

import HarmTrace.Models.Pop.Instances ()


--------------------------------------------------------------------------------
-- From tokens to structured music pieces
--------------------------------------------------------------------------------

pPieceMaj, pPieceMin :: PMusic [Piece]
pPieceMaj = map Piece <$> amb (parseG :: PMusic [Phrase MajMode])
pPieceMin = map Piece <$> amb (parseG :: PMusic [Phrase MinMode])

pPop :: Key -> PMusic [Piece]
pPop (Key _ MajMode) = pPieceMaj
pPop (Key _ MinMode) = pPieceMin


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# OPTIONS_GHC -Wall         #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Audio.DataParser
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Basic parsers for parsing VAMP csv files.
--------------------------------------------------------------------------------

module HarmTrace.Audio.DataParser (
  -- * Parsing beat data
    parseBeatData
  , parseBarTimeData
  , pBeat
  -- * Parsing chromagram data
  , parseChordinoData
  , parseChromaData
  -- * Basic parsers
  , pNumData
  , pComma
  , pParentheticalString
  , pQuotedString
  , pLabel
  -- * Utilities
  , shift
) where

import HarmTrace.Base.MusicTime
import HarmTrace.Base.Parsing hiding (pComma,pQuotedString,pParentheticalString)

--------------------------------------------------------------------------------
-- data parsers
--------------------------------------------------------------------------------

-- | Parsing beat time stamps.
parseBeatData :: Parser BeatTrackerData
parseBeatData = pListSep_ng pLineEnd pLine <* pLineEnd where
  pLine = opt pLabel "" *> pNumData <* opt (pComma *> pQuotedString) ""

-- | Parses 'BarTime' data.
parseBarTimeData :: Parser BarTimeTrackData
parseBarTimeData = pListSep_ng pLineEnd pLine <* pLineEnd where
  pLine = BarTime <$> (opt pLabel "" *> pNumData ) 
                  <*> (pComma *> pBeat)
-- | Parses a 'Beat'.
pBeat :: Parser Beat
pBeat = toBeat <$> pQuotedString where

  toBeat :: String -> Beat
  toBeat "1" = One
  toBeat "2" = Two
  toBeat "3" = Three
  toBeat "4" = Four
  toBeat b   = error ("HarmTrace.Audio.Parser.toBeat: unknown beat " ++ b)

-- | Chroma parsing.
parseChordinoData :: Parser ChordinoData
parseChordinoData =  pListSep_ng pLineEnd pChordinoLine <* pLineEnd where
  pChordinoLine = const convert <$> opt pLabel ""
                                <*> pList1Sep (pSym ',') pNumData 
  convert :: [NumData] -> ChordinoLine -- shift the chorma to match C .. B
  convert l | length l == 25 = ChordinoLine h (shift 3 a) (shift 3 b)
            | otherwise = error ("parseChordinoData: Wrong list length of " 
                                 ++ show (length l))
            where (h:t) = l
                  (a,b) = splitAt 12 t

-- rotates the elements in the list with n positions
shift :: Int -> [a] -> [a]
shift p l = b ++ a where (a,b) = splitAt p l           

-- Parsing 12 dimentional chroma vectors for key-finding.
parseChromaData :: Parser [ChordinoLine]
parseChromaData =  pListSep_ng pLineEnd pCrmLine <* pLineEnd where
  pCrmLine = convert <$> (opt pLabel "" *> pList1Sep (pSym ',') pNumData)
  -- This is a bit of a hack, but I do not want to rewrite all the functions 
  -- again for a very similar data type that only has one 12-dim chroma vector
  convert :: [NumData] -> ChordinoLine   
  convert l | length t == 12 = ChordinoLine h (shift 3 t) [] -- hence we make this []
            | otherwise = error ("parseChromaData: Wrong list length of " 
                                 ++ show (length l))
            where (h:t) = l
                  
                  
--------------------------------------------------------------------------------
-- Basic parsers
--------------------------------------------------------------------------------
                  
pNumData :: Parser NumData
{-# INLINE pNumData #-}
pNumData = pDoubleRaw

pComma :: Parser Char
pComma = pSym ','

pParentheticalString :: Char -> Parser String
pParentheticalString d = pSym d *> pList pNonQuoteVChar <* pSym d where
  pNonQuoteVChar = pSatisfy (\c -> visibleChar c && c /= d) 
                   (Insertion ("Character in a string set off from main text" ++
                    "by delimiter, e.g. double-quotes or comment token") 'y' 5)
  visibleChar c = '\032' <= c && c <= '\126'

pQuotedString :: Parser String
pQuotedString = pParentheticalString '"'

pLabel :: Parser String
pLabel = (pQuotedString `opt` "") <* pComma

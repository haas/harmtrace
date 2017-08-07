{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}

module HarmTrace.HAnTree.HAnParser where

import HarmTrace.Base.Parsing
import HarmTrace.Base.MusicRep (Mode(..))
import HarmTrace.HAnTree.HAn

import Data.Maybe (isJust, fromJust)
--------------------------------------------------------------------------------
-- A Small Parser for Parsing MIR constructors
--------------------------------------------------------------------------------     

-- this top-level function parses a constructor name and returns the 
-- corresponding HAn data type. N.B. we can implement the catch all
-- case as a parser, because it accepts everything and one will get an 
-- "ambiguous parser?" error.s
parseHAn :: ListLike state Char => state -> HAn                          
parseHAn inp 
  | isJust a  = fromJust a
  | otherwise = parseData (HAn 1 <$> pAnyStr) inp where -- catch all case
      a = parseData (pMaybe $ HAnFunc  <$> pHFunc) inp

        
pHFunc :: Parser HFunc
pHFunc =    Ton 1 <$ pSym 'T' <*> pMode <* pSym '_' <*> pInteger <*> pMaybe pSpec
        <|> Dom 1 <$ pSym 'D' <*> pMode <* pSym '_' <*> pInteger <*> pMaybe pSpec
        <|> Sub 1 <$ pSym 'S' <*> pMode <* pSym '_' <*> pInteger <*> pMaybe pSpec
        <|> PD    <$ pString "PD" 
        <|> PT    <$ pString "PT" 

pMode :: Parser Mode
pMode =     MinMode <$ pSym 'm'
        <|> MajMode <$ pString "" 

pSpec :: Parser Spec       
pSpec =     MinBorrow <$ pString "_bor" 
        <|> Blues     <$ pString "_bls" 
        <|> Parallel  <$ pString "_par" 
     
pAnyStr :: Parser String        
pAnyStr =  pAtMost 15 pAscii
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall         #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Audio.AnnotationParser
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Parses textual ground-truth Chord annotations, such as the ones
-- found at: <http://isophonics.net/content/reference-annotations>
--------------------------------------------------------------------------------

module HarmTrace.Audio.AnnotationParser ( parseAnnotationData
                                        , parseKeyAnnotationData
                                        ) where

import HarmTrace.Audio.DataParser (pNumData)
import HarmTrace.Base.MusicTime
import HarmTrace.Base.MusicRep
import HarmTrace.Base.Parsing
import HarmTrace.Base.ChordTokenizer ( pRoot, pChord)

-- perhaps this file should be moved to the tokeniser module, because it is 
-- is very related to tokenising

--------------------------------------------------------------------------------
-- Parse MIREX style chord annotations
--------------------------------------------------------------------------------
-- | Parses a chord annotation.
parseAnnotationData :: Parser [TimedData ChordLabel]
parseAnnotationData =  pListSep_ng pLineEnd pChordSegment <* pLineEnd
                    <* (pLineEnd `opt` "\n")

-- | Parses the onset, offset and chordlabel on one line
pChordSegment :: Parser (TimedData ChordLabel)
pChordSegment = timedData' <$>  pNumData <*   pSpaceTab
                           <*>  pNumData <*   pSpaceTab 
                           <*>  pChord 

--------------------------------------------------------------------------------
-- Keys
--------------------------------------------------------------------------------
-- | Parses a 'Key' annotation.
parseKeyAnnotationData :: Parser [TimedData Key]
parseKeyAnnotationData = noNoneKey <$> pListSep_ng pLineEnd pKeySegment 
                                   <*  pLineEnd where
            -- filter the None keys
            noNoneKey = filter ((/= Note Nothing N) . keyRoot . getData) 


pKeySegment :: Parser (TimedData Key)
pKeySegment = timedData'  <$> pNumData <*  pSpaceTab
                          <*> pNumData <*  pSpaceTab 
                          <*> (pKey    <|> pKeyNone)

pKey :: Parser Key
pKey = Key <$ pString "Key" <* pSpaceTab <*> pRoot <*> pMode

pKeyNone :: Parser Key
pKeyNone = Key (Note Nothing N) MajMode <$ pString "Silence"

pMode :: Parser Mode
pMode  =     MajMode <$ pString ""
         <|> MinMode <$ pString ":minor"
         <|> MinMode <$ pString ":aeolian"
         <|> MajMode <$ pString ":major"
         <|> MinMode <$ pString ":dorian"
         <|> MajMode <$ pString ":mixolydian" -- this must be solved differently
         <|> MajMode <$ pString ":modal"      -- and this too....
         

--------------------------------------------------------------------------------
-- General Parsers and Utils
--------------------------------------------------------------------------------

timedData' :: NumData -> NumData -> a -> TimedData a
timedData' on off chrd = TimedData chrd [Time on, Time off]

pSpaceTab :: Parser Char
pSpaceTab =  pSym ' ' <|> pSym '\t'

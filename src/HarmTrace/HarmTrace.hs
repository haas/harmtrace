{-# LANGUAGE GADTs    #-}
{-# LANGUAGE CPP      #-}
{-# OPTIONS_GHC -Wall #-}

module HarmTrace.HarmTrace ( PPOption(..), Grammar(..), GrammarEx(..)
                           , ParseResult(..), gt2Piece
                           , string2Piece, postProc ) where

import HarmTrace.Models.Models
import HarmTrace.Models.Jazz.Main
import HarmTrace.Models.Pop.Main
import HarmTrace.HAnTree.ToHAnTree
import HarmTrace.HAnTree.Tree
import HarmTrace.HAnTree.HAn (HFunc (P))
import HarmTrace.HAnTree.PostProcess
import HarmTrace.Base.MusicRep
import HarmTrace.Models.ChordTokens as CT
import HarmTrace.Base.ChordTokenizer

import Data.Ord (comparing)
import Data.List (minimumBy)

-- Audio/Annotation Stuff
import HarmTrace.Audio.AnnotationParser

import HarmTrace.Base.Parsing (parseDataWithErrors)
import HarmTrace.Base.MusicTime (dropTimed, getData)

-- Parser stuff
import Text.ParserCombinators.UU hiding ( P )
import Text.ParserCombinators.UU.BasicInstances as PC

--------------------------------------------------------------------------------
-- Plugging everything together
--------------------------------------------------------------------------------

data ParseResult a = ParseResult { parsedKey          :: Key
                                 , parsedChordLabels  :: [ChordLabel]
                                 , parsedPiece        :: [a]
                                 , pieceTreeHAn       :: Tree HAn
                                 , nrAmbTrees         :: Int
                                 , tokenizerErrors    :: [Error LineColPos ]
                                 , pieceErrors        :: [Error Int]
                                 , postProcessing     :: [PPOption]}

-- parses s with string2Piece and merges the deleted chords with the tree
-- (Representable a, GTree (Rep a))
postProc :: (GTree g) => [PPOption] -> ParseResult g -> ParseResult g
postProc opts beforePostProc = beforePostProc { pieceTreeHAn = t }
  where
  t = selectTree $ map (postProcess fs . gTreeHead) (parsedPiece beforePostProc)
  fs = map opt2Func opts
  opt2Func :: PPOption -> (Tree HAn -> Tree HAn)
  opt2Func RemoveInsertions = removeInsertions
  opt2Func RemovePDPT       = removePDPT
  opt2Func MergeDelChords   = mergeDelChords (parsedKey beforePostProc)
                                             (pieceErrors beforePostProc)
                                             (parsedChordLabels beforePostProc)
  opt2Func ExpandChordDurations = expandChordDurations

selectTree :: [Tree HAn] -> Tree HAn
selectTree [] = emptyHAnTree
selectTree ts = minimumBy (comparing getNrFuncNodes) ts

getNrFuncNodes :: Tree HAn -> Int
getNrFuncNodes (Node (HAnFunc P) nodes _) = length nodes
getNrFuncNodes _ = error "HarmTrace.hs: not a correctly formed HAn Tree"

postProcess :: [Tree HAn -> Tree HAn] -> Tree HAn -> Tree HAn
postProcess []     tree = tree
postProcess (f:fs) tree = f (postProcess fs tree)

-- Takes a string with line-separated chords of a song and
-- returns all possible parsed pieces, together with error-correction steps
-- taken (on tokenizing and on musical recognition).
string2Piece :: Grammar g -> String -> ParseResult g
string2Piece g s = let
  (PieceLabel key tok, err) = parse ((,) <$> pSongAbs <*> pEnd)
                                    (createStr (LineColPos 0 0 0) s)
  (trees, err2) = case g of
                    Jazz -> parse_h ((,) <$> pJazz key <*> pEnd)
                              (createStr 0 (toKeyRelTok key tok))
                    Pop  -> parse_h ((,) <$> pPop  key <*> pEnd)
                              (createStr 0 (toKeyRelTok key tok))
  in ParseResult key tok trees emptyHAnTree (length trees) err err2 []


--------------------------------------------------------------------------------
-- Parsing audio file ground-truth annotations
--------------------------------------------------------------------------------

gt2Piece :: (GTree g) => Grammar g -> String -> String -> ParseResult g
gt2Piece g kstr cstr = let
  (ks , errK) = parseDataWithErrors parseKeyAnnotationData kstr
  key         = getData . head $ filter (not . isNone . keyRoot . getData) ks
  (tok, errT) = parseDataWithErrors parseAnnotationData cstr
  ppTok       = dropTimed tok
  (ts, errP)  = case g of
                  Jazz -> parse_h ((,) <$> pJazz key <*> pEnd)
                            (createStr 0 (toKeyRelTok key ppTok))
                  Pop  -> parse_h ((,) <$> pPop  key <*> pEnd)
                            (createStr 0 (toKeyRelTok key ppTok))
  in ParseResult key ppTok ts emptyHAnTree (length ts) (errK ++ errT) errP []

{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HarmTrace.Models.Parser
-- Copyright   :  (c) 2010-2012 Universiteit Utrecht, 2012 University of Oxford
-- License     :  GPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Semi-generic parser for chords
--------------------------------------------------------------------------------

module HarmTrace.Models.Parser (
                                 ParseG (..)
                               , parseGdefault 
                               , PMusic
                               ) where


-- Parser stuff
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances

-- Generics stuff
import Generics.Instant.Base as G

-- Music stuff
import HarmTrace.Models.ChordTokens


--------------------------------------------------------------------------------
-- The generic part of the parser
--------------------------------------------------------------------------------

-- | a type synoniome for a harmonic analysis of a piece of music
type PMusic a = P (Str ChordToken [ChordToken] Int) a

class Parse' f where
   parse' :: PMusic f

instance Parse' U where
  parse' = pure U

instance (ParseG a) => Parse' (Rec a) where
  parse' = Rec <$> parseG

-- Not really necessary because TH is not generating any Var, but anyway
instance (ParseG a) => Parse' (Var a) where
  parse' = Var <$> parseG

instance (Constructor c, Parse' f) => Parse' (G.CEq c p p f) where
  parse' = G.C <$> parse' <?> "Constructor " ++ conName (undefined :: C c f)

instance                              Parse' (G.CEq c p q f) where 
  parse' = empty

instance (Parse' f, Parse' g) => Parse' (f :+: g) where
  parse' = L <$> parse' <|> R <$> parse'

instance (Parse' f, Parse' g) => Parse' (f :*: g) where
  parse' = (:*:) <$> parse' <*> parse'


class ParseG a where
  parseG :: PMusic a

instance (ParseG a) => ParseG [a] where
  parseG = pList1 parseG
  -- We should use non-greedy parsing here, else the final Dom is never parsed
  -- as such.
  -- parseG = pList1_ng parseG

instance (ParseG a) => ParseG (Maybe a) where
  parseG = pMaybe parseG

-- | default generic parser
parseGdefault :: (Representable a, Parse' (Rep a)) => PMusic a
-- parseGdefault = fmap (to . head) (amb parse')
-- Previously we used:
parseGdefault = fmap to parse'
-- This gave rise to many ambiguities. Now we allow parse' to be ambiguous
-- (note that the sum case uses <|>) but then pick only the very first tree
-- from all the possible results. It remains to be seen if the first tree is
-- the best...

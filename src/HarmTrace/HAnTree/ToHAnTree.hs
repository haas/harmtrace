{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}

module HarmTrace.HAnTree.ToHAnTree ( GTree(..) , HAn(..) , gTreeDefault
                                   , gTreeHead , emptyHAnTree ) where

import Generics.Instant.Base
import HarmTrace.HAnTree.Tree (Tree(..))
import HarmTrace.HAnTree.HAn
import HarmTrace.HAnTree.HAnParser

class GTree a where
  gTree :: a -> [Tree HAn] 

instance GTree U where
  gTree U = [Node (HAn 0 "U") [] Nothing]
  
instance (GTree a, GTree b) => GTree (a :+: b) where
  gTree (L x) = gTree x
  gTree (R x) = gTree x
  
instance (GTree a, Constructor c) => GTree (CEq c p q a) where
  gTree c@(C a) = [Node (parseHAn (conName c)) (gTree a) Nothing]
    
instance (GTree a, GTree b) => GTree (a :*: b) where
  gTree (a :*: b) = gTree a ++ gTree b

instance GTree a => GTree (Rec a) where
  gTree (Rec x) = gTree x
  
instance GTree a => GTree (Var a) where
  gTree (Var x) = gTree x


instance GTree a => GTree [a] where
  gTree x = concatMap gTree x

-- Dispatcher
gTreeDefault :: (Representable a, GTree (Rep a)) => a -> [Tree HAn]
gTreeDefault = gTree . from

gTreeHead :: (GTree a) => a -> Tree HAn
gTreeHead = head . gTree

emptyHAnTree :: Tree HAn
emptyHAnTree = Node (HAn 0 "empty") [] Nothing
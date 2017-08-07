
module HarmTrace.HAnTree.Tree where

import Data.Maybe
import qualified Data.Binary as B
import Control.Monad.State
import Data.List (maximumBy, genericLength)
import Control.DeepSeq

--------------------------------------------------------------------------------
-- Tree datastructure
--------------------------------------------------------------------------------

-- our (temporary) tree data structure, a leaf is simply represented
-- as Node a [] or NodePn a [] pn
data Tree a = Node { getLabel :: !a, getChild :: ![Tree a], getPn :: !(Maybe Int) }
  deriving Eq
  
-- specific show instance for pretty printing
instance (Show a) => Show (Tree a) where 
  show (Node a children _) =  --filter (\x -> '\"' /= x && '\'' /= x) 
                            desc where
    desc = ('[' : show a) ++ concatMap show children ++ "]"

instance (NFData a) => NFData (Tree a) where
  rnf (Node l c p) = rnf l `seq` rnf c `seq` rnf p

instance (B.Binary a) => B.Binary (Tree a) where
  put (Node l c p) = B.put l >> B.put c >> B.put p
  get = liftM3 Node B.get B.get B.get
    
--------------------------------------------------------------------------------
-- Creating Trees
--------------------------------------------------------------------------------    
    
-- returns a Tree data structure, given a string representation of a tree.
strTree :: String -> Tree String 
strTree = head . strTree' where
  strTree' [] = []
  strTree' (c:cs) 
    | c == '['  = Node lab (strTree' a) Nothing : strTree' b
    | c == ']'  = strTree' cs
    | otherwise = error ("cannot parse, not well formed tree description: " 
                ++ [c]) where 
                  (x  ,b) = splitAt (findClose cs) cs
                  (lab,a) = span (\y -> (y /= '[') && (y /= ']')) x

-- given a string representation of a tree, e.g. "[b][c]]" findClose
-- returns the index of the closing bracket, e.g. 6.
findClose  :: String -> Int    
findClose  s  = findClose' s 1 0
findClose' :: String -> Int -> Int -> Int 
findClose' [] b ix
  | b == 0    = ix-1
  | otherwise = error 
                 "not well formed tree description: cannot find closing bracket"
findClose' (c : cs) b ix
  | b == 0    = ix-1 
  | c == '['  = findClose' cs (b+1) (ix+1)
  | c == ']'  = findClose' cs (b-1) (ix+1)
  | otherwise = findClose' cs  b    (ix+1) 
   
--------------------------------------------------------------------------------
-- Basic operations on the Tree data structure
--------------------------------------------------------------------------------
    
-- given a list with tree getPn
getPns :: [Tree t] -> [Int]
getPns = map (fromJust . getPn)     

-- returns the list of po nrs of the children of t
getChildPns :: Tree a -> [Int]
{-# INLINE getChildPns  #-} 
getChildPns (Node _lab children _pn) = map (fromJust . getPn) children

-- returns the subtree of t given its post order number pn
getSubTree :: Tree t -> Int -> Tree t
getSubTree t pn = pot t!!pn

-- returns True if t is a leaf and False otherwise
isLf :: (Eq t) => Tree t -> Bool
isLf t = getChild t == []

collectLeafs :: Tree t -> [Tree t]
collectLeafs t@(Node _ [] _) = [t]
collectLeafs   (Node _ cn _) = concatMap collectLeafs cn 

-- returns the size of the tree
size, depth :: Tree t -> Int
size (Node _ [] _) = 1
size (Node _ children _ ) = foldr ((+) . size ) 1 children

-- returns the size of a forrest of trees
sizeF, depthF :: [Tree t] -> Int
sizeF treeList = foldr ((+) . size ) 0 treeList

avgDepth :: Tree t -> Float
avgDepth t = fromIntegral (sum dep) / (genericLength dep) where 
  dep = depth' 1 t

avgDepthF :: [Tree t] -> Float
avgDepthF t = let l = map avgDepth t in sum l / genericLength l 

-- returns the maximum depth of a tree
depth t = maximumBy compare (depth' 1 t)

-- returns the maximum depth of a forrest of trees
depthF treeList = maximumBy compare (concatMap (depth' 1) treeList) 

-- depth helper
depth' :: Int -> Tree t -> [Int]
depth' x (Node _ [] _ ) = [x]
depth' x (Node _ c  _ ) = x : concatMap (depth' (x+1)) c

-- recursively removes the nodes with label 'x' from a tree
remove :: (Eq t) => t -> Tree t -> Tree t
remove x = removeBy (== x)

-- more general version of remove
removeBy :: (t -> Bool) -> Tree t -> Tree t
removeBy f t = head (removeBy' f t)
removeBy' :: (t -> Bool) -> Tree t -> [Tree t]
removeBy' f (Node l c pn) 
  | f l       = concatMap (removeBy' f) c 
  | otherwise = [(Node l (concatMap (removeBy' f) c) pn)]

-- collects all the subtrees of tree in a list in post order.
pot, pot', pret, pret',potPret :: Tree t -> [Tree t]
potPret t                    =  pot' (setPre  t)
pot    t                     =  pot' (setPost t)
pot'   t@(Node _ [] _)       =  [t]
pot'   t@(Node _ children _) =  concatMap pot' children ++ [t]
-- collects all the subtrees of tree in a list in pre order.
pret    t                    =  pret' (setPre t)
pret'  t@(Node _ [] _)       =  [t]
pret'  t@(Node _ children _) =  t : concatMap pret' children

-- very inefficient way of converting a pre order number to a post order number
-- just for testing....
preToPost :: Tree t -> Int -> Int
preToPost t pn = fromJust . getPn $ pret' (setPost t) !! pn


-- Converts Node's to NodePn's and sets the post order numbers
-- JPM: setPost is a typical tree labelling problem.
-- Looks nicer with the state monad, I think:
setPost, setPre :: Tree t -> Tree t
setPost t = evalState (stm t) 0 where
  stm :: Tree t -> State Int (Tree t)
  stm (Node a cs _) = do cs' <- mapM stm cs
                         pn  <- get
                         modify (+1)
                         return (Node a cs' (Just pn))    

-- Sets pre order numbers 
setPre t = evalState (stm t) 0 where
  stm :: Tree t -> State Int (Tree t)
  stm (Node a cs _) = do pn  <- get
                         modify (+1)
                         cs' <- mapM stm cs
                         return (Node a cs' (Just pn))                

--not very efficient, but nevertheless very effective, todo optimize elem operation
matchToTree :: Tree t -> [Int] -> [Tree t]
matchToTree t@(Node _ _ Nothing ) k = matchToTree (setPost t) k
matchToTree   (Node a cn (Just pn)) k =
  let cs = concatMap (`matchToTree` k) cn
  in if pn `elem` k then [Node a cs (Just pn)] else cs                         

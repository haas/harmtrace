{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HarmTrace.IO.Match ( 

    MatchMode(..), dirMatch

  ) where


-- Common IO functions
import HarmTrace.IO.Common

-- Music stuff
import HarmTrace.HarmTrace
import HarmTrace.Base.MusicRep
import HarmTrace.Models.Jazz.Instances ()
import HarmTrace.HAnTree.Tree (Tree)
import HarmTrace.HAnTree.ToHAnTree
import HarmTrace.IO.Errors
import HarmTrace.Matching.Standard
import HarmTrace.Matching.GuptaNishimura
import HarmTrace.Matching.Alignment (getAlignDist, getHAnDist)

-- Library modules
import System.FilePath
import Data.Maybe (isJust, fromJust)
import Data.Binary

-- Parallelism
import Control.Parallel.Strategies


--------------------------------------------------------------------------------
-- Symbolic Matching IO
--------------------------------------------------------------------------------

data MatchMode = STDiff | LCESsize | LCESsim | HAnAlign | Align
  deriving (Eq, Ord, Show)  

-- should return True if sim a b == sim b a and False otherwise  
isSymmetrical :: MatchMode -> Bool
-- @pedro: I guess it is symmetrical, but I'm not 100% sure
isSymmetrical STDiff    = False 
isSymmetrical LCESsize  = True
isSymmetrical LCESsim   = True
isSymmetrical HAnAlign  = True  
isSymmetrical Align     = True  
  
-- matches a directory of chord description files
dirMatch :: (GTree g)
         => Grammar g -> [PPOption] -> Maybe FilePath
         -> MatchMode -> Maybe Float -> FilePath -> IO ()
dirMatch g o bIn m me fp = 
  do fs <- readDataDir fp
     let process s   = let (ParseResult _ tks _ ts _nrts _ ePar _) 
                              = postProc o $ string2Piece g s
                       in  (tks, ts, errorRatio ePar tks)
         filterError = if isJust me 
                          then filter (\(_,_,e) -> e <= fromJust me) else id
     pss <- mapM (\f -> readFile' (fp </> f)) fs
     (tks, ps) <- case bIn of
                    Just bp -> decodeFile bp :: IO ([[ChordLabel]],[Tree HAn])
                    Nothing -> let (toks, ps', _) = unzip3 (filterError 
                                                             (map process pss))
                               -- the line below gives an compile error in ghci
                               in return (toks, ps' `using` parList rdeepseq)
                               -- in return (toks, ps')
     let fsQLab = labelQuery fs
     -- print the ireval format ...
     putStr "true\n"
     if (m == LCESsize || m == LCESsim || m == HAnAlign || m == Align) 
        then putStr "false\n" else putStr "true\n"
     mapM_ (putStr . (++ "\t"). showIntWithZeros . getId . getInfo) 
           (fst . unzip $ filter snd fsQLab) 
     putChar '\n'
     mapM_ (putStr . (++ "\t"). showIntWithZeros . getId . getInfo) fs 
     putChar '\n'
     -- do the actual matching ... 
     let match :: (a -> a -> Float) -> [a] -> [([Float],Bool)]
         match sim l = [ ([ calcSim sim x y i j
                       | (j,y)      <- zip  [0..] l], xIsQ) -- :: ([Float],Bool)
                       | (i,x,xIsQ) <- zip3 [0..] l (snd . unzip $ fsQLab)]
         -- calculate the similarity sim a b, or, if calculated, look up sim b a                         
         calcSim :: (a -> a -> Float) -> a -> a -> Int -> Int -> Float
         calcSim sim x y i j = if isSymmetrical m && j < i 
                               then (fst (simMat !! j)) !! i else sim x y
         simMat, querySimMat :: [([Float],Bool)]
         simMat = (case m of -- full n x n similarity matrix
                    STDiff    -> match diffChordsLen tks
                    LCESsize  -> match getLCESsize ps
                    LCESsim   -> match getLCESsim  ps
                    HAnAlign  -> match getHAnDist  ps
                    Align     -> match (getAlignDist tempKeyC tempKeyC) tks  
                  )  where tempKeyC = (Key (Note Nothing C) MajMode)
         -- filter all non-queries, lazy evaluation should ensure the 
         -- non-queries will not be evaluated
         querySimMat = (filter snd simMat) `using` parList rdeepseq
     sequence_ [ printLine x | (x,_) <- querySimMat]
     
printLine :: [Float] -> IO ()     
printLine l  = printLn (foldr (\a b -> showFloat a ++ "\t" ++ b) "" l)    
     
-- labels (True/False) the songs that have multiple versions and are queries
labelQuery :: [FilePath] -> [(FilePath, Bool)]
labelQuery l = let cs = getClassSizes l in 
  map (\x -> (x,(>1) . length . fromJust $ lookup (getTitle $ getInfo x) cs)) l 

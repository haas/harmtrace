{-# OPTIONS_GHC -Wall #-}
module HarmTrace.IO.PrintTree ( printTreeF   , printTree
                              , printTreeHAnF, printTreeHAn) where

import System.Exit (ExitCode)
import System.Process (runProcess, waitForProcess, ProcessHandle)
import HarmTrace.HAnTree.Tree (Tree)
import HarmTrace.HAnTree.HAn (HAn)


-- needs gnu wget http://www.gnu.org/software/wget/ 
-- or             http://gnuwin32.sourceforge.net/packages/wget.htm

printTreeHAn :: Tree HAn -> FilePath -> IO ExitCode
printTreeHAn t o = printTree (show t) (o ++ ".png")

printTreeHAnF :: [Tree HAn] -> String -> IO ExitCode
printTreeHAnF ts o = printTreeF (map show ts) o


-- |gets a .png from http://ironcreek.net/phpsyntaxtree/ that prints
-- the parse tree of the chord sequence that was entered as parsed by our 
-- HarmGram model. wget is used under the hood. If any, the first ten 
-- ambiguous parse trees are printed.                
printTreeF :: [String] -> -- ^ the tree description to be printed   
              String   -> -- ^ a string for generating the filenames            
              IO ExitCode      
printTreeF trees  outStr   =    printMoreTrees (take 15 trees) outStr 0 where
  printMoreTrees :: [String] -> String -> Int -> IO ExitCode
  printMoreTrees []     _   _ = error "empty list of trees, nothing to print"
  printMoreTrees [t]    out i = printTree t (nrFile out i) 
  printMoreTrees (t:ts) out i = do _ <- printTree t (nrFile out i) 
                                   printMoreTrees ts out  (i+1)
  nrFile :: String -> Int -> String                                      
  nrFile str i = str ++ show i ++ ".png"    
  
-- |gets a .png from http://ironcreek.net/phpsyntaxtree/ that prints
-- the parse tree of the chord sequence that was entered as parsed by our 
-- HarmGram model. wget is used under the hood. If any, the first ten 
-- ambiguous parse trees are printed.                           
printTree :: String ->   -- ^ the tree description to be printed
             FilePath -> -- ^ a filepath to the output file
             IO ExitCode  
printTree tree outfile  = do submit <- submitTree tree 
                             _ <- waitForProcess submit 
                             png <- getpng outfile 
                             waitForProcess png

-- wget --save-cookies cookies.txt 
        --keep-session-cookies 
        --post-data "data=[tree]&drawbtn=&opencount=3&closedcount=3&font=vera_sans&fontsize=8&color=on&antialias=on&autosub=on&triangles=on" 
        --http://ironcreek.net/phpsyntaxtree/ 
submitTree :: String -> IO ProcessHandle
submitTree tree = wget "http://ironcreek.net/phpsyntaxtree/" opt  where
  opt = [ "--save-cookies"
        , "phpsyntax_cookies.txt"
        , "--keep-session-cookies"
        , "--quiet"
        , "--delete-after"
        , "--post-data"
        , "data=" ++ tree ++ "&drawbtn=&opencount=3&closedcount=3&" ++
          "font=vera_sans&fontsize=8&color=on&antialias=on&triangles=on"
        ]
        
--        
--wget  cookies.txt --output-document %2.png http://ironcreek.net/phpsyntaxtree/dnlgraph.php 
getpng :: String -> IO ProcessHandle
getpng name = wget "http://ironcreek.net/phpsyntaxtree/dnlgraph.php" opt  where
  opt = [ "--load-cookies"
        , "phpsyntax_cookies.txt"
        , "--quiet"
        , "--output-document"
        , name
        ]
        
wget :: String -> [String] -> IO ProcessHandle
wget url opt = 
  do let par = url : opt
     runProcess "wget" par Nothing Nothing Nothing Nothing Nothing


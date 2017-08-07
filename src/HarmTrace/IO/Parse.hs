{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HarmTrace.IO.Parse (

    parseTree, parseTreeVerb, parseDir,
    parseAnnotation, parseAnnotationVerb, parseAnnotationDir,

  ) where


-- Common IO functions
import HarmTrace.IO.Common

-- Parser stuff
import Text.ParserCombinators.UU

-- Music stuff
import HarmTrace.HarmTrace
import HarmTrace.Base.MusicRep
import HarmTrace.Models.Jazz.Instances ()
import HarmTrace.HAnTree.Tree (Tree)
import HarmTrace.HAnTree.ToHAnTree
import HarmTrace.IO.Errors

-- Library modules
import Data.List (sort, intersperse, genericLength)
import System.FilePath
import System.Directory
import System.CPUTime
import Data.Binary


--------------------------------------------------------------------------------
-- Symbolic Parsing IO 
--------------------------------------------------------------------------------

-- | Parses a string of chords and returns a parse tree with the harmony structure
parseTree, parseTreeVerb :: (GTree g) => Grammar g -> [PPOption] -> String 
                         -> IO (ParseResult g)
parseTree g opts s =
  do let pr@(ParseResult _ tks _ _ n te pe _) = postProc opts $ string2Piece g s
     putStrLn ("parsed " ++ show (length tks) ++ " chords into " 
                         ++ show n            ++ " analysis tree(s)")
     if not $ null te then showErrors "tokenizer: " te 
                     else putStr ""
     if not $ null pe then showErrors "parser: " pe 
                     else putStr ""
     return pr

parseTreeVerb g opts s = 
  do let pr@(ParseResult _ tks _ _ n te pe _) = postProc opts $ string2Piece g s
     putStrLn ("parsed " ++ show (length tks) ++ " chords into " 
                         ++ show n            ++ " analysis tree(s)")
     if not $ null te then mapM_ print te
                      else putStr ""
     if not $ null pe then mapM_ print pe
                      else putStr ""
     return pr                 
      
-- | Batch analyses a directory with chord sequence files and presents the
-- user some reduced output.
parseDir :: (GTree g)
         => Grammar g -> [PPOption] -> FilePath -> Maybe FilePath -> IO ()
parseDir g opts filepath bOut =     getDirectoryContents filepath 
                                >>= parseDir' g opts bOut filepath . sort

parseDir' :: (GTree g)
          => Grammar g -> [PPOption] -> Maybe FilePath
          -> String -> [String] -> IO ()
parseDir' g opts bOut fp fs = 
  do putStr "Filename\tNumber of trees\t"
     putStr "Insertions\tDeletions\tReplacements\tDeletions at the end\t"
     putStr "Tot_Correction\tNr_of_chords\t"
     putStrLn "Error ratio"
     --putStrLn "Error ratio\tTime taken"
     let process :: FilePath -> FilePath -> IO ([ChordLabel],Tree HAn, Int)
         process path x =
          do content <- readFile (path </> x)
             let (ParseResult _ tks ps ts nr e1 e2 _) 
                      = postProc opts $ string2Piece g content
                 -- @Pedro: I think that the (length ts) only is here to 
                 -- evaluate all trees right? Since the tree selection is now
                 -- incorporated in the postprocessing I replaced it with 
                 -- length ps
                 -- t                = seq (length ts) (return ())
                 t                = seq (length ps) (return ())
                 ErrorNrs i d e r = countErrors e2
                 errRat           = errorRatio e2 tks
                 nrOfChords       = length tks -- (mergeDups toks)
             t1 <- getCPUTime
             t
             t2 <- getCPUTime
             let _diff = fromIntegral (t2 - t1) / (1000000000 :: Float)
             when (not $ null e1) $ putErrLn (show x ++ ": " ++ show e1)
             printLn . concat $ intersperse "\t" [ x, show nr
                                       , show i, show d, show r, show e
                                       , show (i+d+e+r)
                                       , show nrOfChords, showFloat errRat
                                        -- JPM: do not print the time taken so
                                        -- as to allow diffing output in tests
                                       ] -- , showFloat diff]
             return (tks, ts, i+d+e+r)
     res <- mapM (process fp) (filter ((== ".txt") . takeExtension) fs)
     let (cs,ts,err) = (unzip3 res :: ([[ChordLabel]],[Tree HAn], [Int]))
     printLn ("average error: " ++ 
               show ((fromIntegral $ sum err) / (genericLength err) :: Double))
     case bOut of
       Nothing -> return ()
       Just bf -> encodeFile bf (cs, ts)

--------------------------------------------------------------------------------
-- Audio Ground-truth annotations IO
-------------------------------------------------------------------------------- 

parseAnnotation, parseAnnotationVerb :: (GTree g) => Grammar g -> [PPOption] 
   -> String -> String -> IO (ParseResult g)
-- | Parses a ground-truth chord annotation in standard MIREX format and 
-- presents the user some condensed parsing statistics
parseAnnotation g opts k ann =
  do let pr@(ParseResult _ tks _ _ n te pe _) = postProc opts $ gt2Piece g k ann  
     putStrLn ("key: "   ++ k)
     putStrLn ("parsed " ++ show (length tks)++ " audio chord annotations into " 
                         ++ show n           ++ " ambiguous trees")
     when (not $ null te) (showErrors "tokenizer: " te)
     when (not $ null pe) (showErrors "parser: "    pe)
     return pr

-- | Parses a ground-truth chord annotation in standard MIREX format and 
-- presents the user somewhat more elaborate parsing statistics
parseAnnotationVerb g opts k ann =
  do let pr@(ParseResult _ tks _ _ n te pe _) = postProc opts $ gt2Piece g k ann  
     putStrLn ("key: "   ++ k)
     when (not $ null te) (do putStrLn "tokenizer errors:" ; mapM_ print te)
     when (not $ null pe) (do putStrLn "parser errors:"    ; mapM_ print pe)
     putStrLn ("parsed " ++ show (length tks)++ " audio chord annotations into " 
                         ++ show n           ++ " ambiguous trees")
     return pr   

-- | Parses a directory of annotation files and key description files
-- and prints condensed parsing information to std out
parseAnnotationDir :: GTree a => Grammar a -> [PPOption] -> FilePath -> FilePath
                   -> IO ()     
parseAnnotationDir g opts kdir andir =
  do ks  <- getDirectoryContents kdir
     ans <- getDirectoryContents andir
         -- prints parse results in one line
     let prntParse :: (FilePath,FilePath) -> IO ()
         prntParse (kfp,anfp) = 
           do k <- readFile kfp
              a <- readFile anfp
              printLn . concat $ intersperse "\t" (takeFileName kfp 
                : (showParseResult . postProc opts $ gt2Piece g k a))
         -- filters .lab files and adds the path
         fileFilter :: FilePath -> [FilePath] -> [FilePath]
         fileFilter pf = map (combine pf) . filter ((== ".lab") . takeExtension)       

     case matchKeyAnn (fileFilter kdir  $ reverse ks) 
                      (fileFilter andir $ reverse ans) of
       Just x  -> do printVersion
                     printLn ("Filename\tkey\tnrOfTrees\tInsertions\tDeletions"
                           ++ "\tDelsAtEnd\tTotalErr\tnrOfChords\tTokenizerErr")
                     mapM_ prntParse x
       Nothing -> putStrLn ("the filenames in " ++ kdir 
                         ++ " do not exactly match the ones in " ++ andir)
                         
-- shows some elements of a ParseResult 
showParseResult :: ParseResult a -> [String]
showParseResult (ParseResult k tk _p _han n te pe _pp) =
  let pErr   = countErrors pe 
      insert = ins pErr
      delete = del pErr
      endDel = delEnd pErr
      total  = insert + delete + endDel
  -- key nrOfTrees Insertions Deletions DelsAtEnd TotalErr tokenizerErr
  in show k : (map show (n : insert : delete : endDel : total 
                           : length tk : length te :[]))


-- Checks if the key and the annotation files all match, if this is the
-- case it will return a paired list of these files
matchKeyAnn :: [FilePath] -> [FilePath] -> Maybe [(FilePath,FilePath)]     
matchKeyAnn ks ans = 
  let match = and $ zipWith eqFileName ks ans
      eqFileName :: FilePath -> FilePath -> Bool
      eqFileName a b = takeFileName a == takeFileName b
  in if match then Just $ zip ks ans else Nothing
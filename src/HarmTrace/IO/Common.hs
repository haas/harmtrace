{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HarmTrace.IO.Common (

    showFloat, showIntWithZeros,
    getClassSizes, getTitle, getInfo, getId, 
    readFile', hGetContents', readDataDir,
    printLn, putErrLn, printVersion, fileExists, dirExists

  ) where

-- Parser stuff
import Text.ParserCombinators.UU

-- Music stuff
import HarmTrace.Models.Jazz.Instances ()
import HarmTrace.Base.Parsing
import Constants (vERSION)

-- Library modules
import Data.List (sort, groupBy)
import Control.Arrow ((***))
import System.FilePath
import System.Directory
import System.IO
import Text.Printf (printf)


--------------------------------------------------------------------------------
-- Data set Info
--------------------------------------------------------------------------------

-- parses a database name
pDB :: Parser String
pDB = pString "_" *> (   pString "allanah" 
                     <|> pString "wdick" 
                     <|> pString "community" 
                     <|> pString "midicons" 
                     <|> pString "realbook" ) <* pString "." 

-- parses a biab extension: "(M|S|m|s)(G|g)[0-9A-Za-z]{1}.txt"
pBiabExt :: Parser String
pBiabExt =   (:) <$> (pSym 'M' <|> pSym 'S' <|> pSym 'm' <|> pSym 's') <*>
            ((:) <$> (pSym 'G' <|> pSym 'g') <*> 
            ((:) <$>  pAscii <*> (pure []))) <* pString ".txt"

-- parses a title (everything upto "_id_")
pTitle :: Parser String
pTitle = pManyTill pAscii (pString "_id_")

-- parses the complete Biab filename
pBiab :: Parser BiabInfo
pBiab = BiabInfo <$> pTitle <*> pIntegerRaw <*> pDB <*> pBiabExt

-- a datatype for storing the band-in-a-box filename information
data BiabInfo = BiabInfo { getTitle :: String
                         , getId    :: Int
                         , _getDb   :: String
                         , _getExt  :: String }

-- gets the groundtruth information from the filename by parsing it
getInfo :: FilePath -> BiabInfo
getInfo = parseData pBiab

-- for debugging
-- getInfoErrors :: FilePath -> BiabInfo
-- getInfoErrors fp = let (a, err) = parseDataWithErrors pBiab fp 
                   -- in if not . null $ err then traceShow err a else a

-- shows an integer always with 5 numbers, e.g. 45 -> 00045
-- The function can very probably be replaced by "show", but than we have 
-- to change the tests, and make sure it realy does not make a difference
showIntWithZeros :: Int -> String
showIntWithZeros i = let i' = show i 
                     in take (5 - (length i')) (repeat '0') ++ i'

-- creates a (id, title) mapping from a list of files
createGroundTruth :: [String] -> [(String, String)]
createGroundTruth files = [ (getTitle x, showIntWithZeros $ getId x) 
                           | x <- map getInfo files       ]

getClassSizes :: [String] -> [(String,[String])]
getClassSizes = map ((head *** id) . unzip) . groupBy gf . createGroundTruth
  where gf (name1, _key1) (name2, _key2) = name1 == name2

-- writeGroundTruth :: FilePath -> FilePath -> IO ()
-- writeGroundTruth infp outfp =
  -- do  files <- readDataDir infp
      -- writeFile outfp . Prelude.tail $ concatMap merge (createGroundTruth files) 
         -- where merge :: (String, String) -> String
               -- merge (x,y) = '\n' : y ++ "\t" ++ x

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
putErrLn :: String -> IO()
putErrLn = hPutStrLn stderr

printLn :: String ->IO ()
printLn s = putStrLn s >> hFlush stdout

-- Stricter readFile
hGetContents' :: Handle -> IO [Char]
hGetContents' hdl = do e <- hIsEOF hdl
                       if e then return []
                         else do c <- hGetChar hdl
                                 cs <- hGetContents' hdl
                                 return (c:cs)

readFile' :: FilePath -> IO [Char]
readFile' fn = do hdl <- openFile fn ReadMode
                  xs <- hGetContents' hdl
                  hClose hdl
                  return xs
                  
readDataDir :: FilePath -> IO [FilePath]
readDataDir fp = 
  do fs <- getDirectoryContents fp
     return . sort $ filter (\str -> ".txt" == takeExtension str ) fs

-- | Shows a Float with 5 decimal places
showFloat :: Float -> String
showFloat = printf "%.6f" 

printVersion :: IO ()
printVersion = putStrLn vERSION

-- | extends the 'System.Directory.doesFileExist' by printing an error message
-- if the file does not exist.
fileExists :: FilePath -> IO (Bool)
fileExists fp = 
  do e <- doesFileExist fp  
     when (not e) $ putStrLn ("Error: file does not exists: " ++ show fp)
     return e
     
-- | extends the 'System.Directory.doesDirectoryExist' by printing an error 
-- message if the file does not exist.
dirExists :: FilePath -> IO (Bool)
dirExists fp = 
  do e <- doesDirectoryExist fp  
     when (not e) $ putStrLn ("Error: directory does not exists: " ++ show fp)
     return e
     

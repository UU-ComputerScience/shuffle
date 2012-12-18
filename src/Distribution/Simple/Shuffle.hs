module Distribution.Simple.Shuffle (shuffleHooks) where

import Distribution.Simple (UserHooks (..))
import Distribution.Simple.PreProcess (PreProcessor (..), mkSimplePreProcessor)
import Distribution.PackageDescription (PackageDescription (..), BuildInfo (..), Executable (..),
                                        Library (..), TestSuite (..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Simple.Utils (die, warn, info, notice, findFileWithExtension', 
                                  createDirectoryIfMissingVerbose)
import Distribution.Simple.Setup (BuildFlags(..), fromFlagOrDefault)
import Distribution.Verbosity (Verbosity, normal, silent)
import Distribution.ParseUtils (runP, parseOptCommaList, parseFilePathQ, ParseResult (..))
import Distribution.ModuleName (fromString, ModuleName)

import Control.Monad (forM, forM_, when)
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.List ((\\), union, intersect, nub, intercalate)
import System.IO (openFile, IOMode(..), hClose, withFile, hFileSize)
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeExtension, dropExtension, replaceExtension,
                        normalise, pathSeparator, dropFileName)

import UHC.Util.FPath (FPath, fpathGetModificationTime, fpathFromStr)
import UHC.Shuffle (shuffleCompile, parseOpts, getDeps, Opts, FPathWithAlias)

-- | Add shuffle to a set of existing userhooks. To use shuffle together
-- with UUAGC, define a Setup.hs as follows:
--
-- > import Distribution.Simple (defaultMainWithHooks)
-- > import Distribution.Simple.Shuffle (shuffleHooks)
-- > import Distribution.Simple.UUAGC (uuagcLibUserHook)
-- > import UU.UUAGC (uuagc)
-- >
-- > main :: IO ()
-- > main = defaultMainWithHooks (shuffleHooks (uuagcLibUserHook uuagc))
--
-- For .chs files, the shuffle settings can be configured in the
-- .cabal file as follows:
--
-- >   x-shuffle-hs:        --gen-reqm=1 --preamble=no --lhs2tex=no --variant-order="1"
--
-- For the .cag files, the shuffle and AG options can be specified as:
--
-- >   x-shuffle-ag:        --gen-reqm=1 --preamble=no --lhs2tex=no --variant-order="1"
-- >   x-shuffle-ag-d:      data, rename
-- >   x-shuffle-ag-s:      catas, semfuns, signatures, pretty, rename
-- >   x-shuffle-ag-sd:     data, catas, semfuns, signatures, pretty, rename, module
-- >   x-shuffle-ag-d-dep:  Data/DataFile.cag
-- >                        Another.cag
-- >   x-shuffle-ag-s-dep:  Main.cag
-- >                        Data/Imports.cag
-- >                        Another.cag
--
shuffleHooks :: UserHooks -> UserHooks
shuffleHooks h = h {
  buildHook = shuffleBuildHook (buildHook h),
  hookedPreProcessors = ("chs", chsPrep) : hookedPreProcessors h }

chsPrep :: BuildInfo -> LocalBuildInfo -> PreProcessor
chsPrep buildInfo localBuildInfo = PreProcessor {
  platformIndependent = True,
  runPreProcessor = mkSimplePreProcessor (\i o v -> preprocess buildInfo "hs" i o v >> return ()) }

parseFileList :: String -> String -> Verbosity -> IO [FilePath]
parseFileList fieldName field verbosity =
  case runP 0 fieldName (parseOptCommaList parseFilePathQ) field of
    ParseFailed err    -> die $ show err
    ParseOk warnings r -> mapM_ (warn verbosity . show) warnings >> return r

toModuleName :: FilePath -> ModuleName
toModuleName = fromString . map (\x -> if x == pathSeparator then '.' else x) . dropExtension

generateAG :: FilePath -> BuildInfo -> Verbosity -> [String] -> IO [ModuleName]
generateAG outDir bi verbosity files = do
  -- Find all cag files and their dependencies
  deps <- forM files $ \inFile -> do
    mbPath <- findFileWithExtension' [takeExtension inFile] (hsSourceDirs bi) (dropExtension inFile)
    case mbPath of
      Nothing -> die $ "can't find source for " ++ inFile ++ " in " ++ intercalate ", " (hsSourceDirs bi)
      Just (dir,file) -> do
        -- Preprocess this file
        let outFile = outDir </> replaceExtension file "ag"
        empt <- preprocess bi "ag" (normalise $ dir </> file) outFile verbosity
        if empt
          then return (Nothing, [])
          else do
            -- Construct modulename to export
            let modName = toModuleName file
            -- Find dependencies
            (opts, _, _) <- getOpts silent bi "dep" ["--depbase=" ++ dir] file
            deps' <- getDeps opts file
            let deps'' = map (\dep -> (dir,replaceExtension dep "cag")) deps'
            return $ (Just modName, deps'')
  -- Preprocess all dependencies
  forM_ (nub $ concat $ map snd deps) $ \(inDir,inFile) -> do
    let outFile = outDir </> replaceExtension inFile "ag"
    preprocess bi "ag" (normalise $ inDir </> inFile) outFile verbosity
  forM_ (map fst deps) print
  -- Return all extra modules that should be build
  return $ catMaybes $ map fst deps

shuffleBuildHook :: (PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()) -> PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
shuffleBuildHook origBuildHook pd lbi hook bf = do
  let verbosity = fromFlagOrDefault normal (buildVerbosity bf)
  let addOpts :: FilePath -> BuildInfo -> IO BuildInfo
      addOpts outDir bi = do
        -- Read options from cabal and settings file
        let fields = customFieldsBI bi
        -- Get data files
        dataFiles <- case "x-shuffle-ag-d-dep" `lookup` fields of
          Just files -> parseFileList "x-shuffle-ag-d-dep" files verbosity
          _          -> return []
        -- Get sem files
        semFiles <- case "x-shuffle-ag-s-dep" `lookup` fields of
          Just files -> parseFileList "x-shuffle-ag-s-dep" files verbosity
          _          -> return []
        -- Passing different options to UUAG
        let extraOpts name files = case name `lookup` fields of
              Just opts -> forM files $ \file -> do
                let fullName = outDir </> replaceExtension file "ag"
                return ("x-agmodule", "file : " ++ show fullName ++ " options : " ++ opts)
              _ -> return []
        -- Set all options for data files
        dataOpts <- extraOpts "x-shuffle-ag-d" (dataFiles \\ semFiles)
        semOpts <- extraOpts "x-shuffle-ag-s" (semFiles \\ dataFiles)
        semDataOpts <- extraOpts "x-shuffle-ag-ds" (semFiles `intersect` dataFiles)
        -- Now generate all ag files
        let allFiles = semFiles `union` dataFiles
        extraModules <- generateAG outDir bi verbosity allFiles
        -- Update the corresponding fields
        return $ bi { customFieldsBI = dataOpts ++ semOpts ++ semDataOpts ++ customFieldsBI bi 
                    , otherModules = extraModules ++ otherModules bi 
                    , hsSourceDirs = outDir : hsSourceDirs bi }
  -- Add all options and continue with original hook
  exes <- forM (executables pd) $ \exe -> do
    newBi <- addOpts (buildDir lbi </> exeName exe </> exeName exe ++ "-tmp") (buildInfo exe)
    return $ exe { buildInfo = newBi }
  lib <- case library pd of
    Just l -> do
      newBi <- addOpts (buildDir lbi) (libBuildInfo l)
      return $ Just $ l { libBuildInfo = newBi }
    Nothing -> return Nothing
  tests <- forM (testSuites pd) $ \test -> do
    newBi <- addOpts (buildDir lbi </> testName test </> testName test ++ "-tmp") (testBuildInfo test)
    return $ test { testBuildInfo = newBi }
  origBuildHook (pd { executables = exes, library = lib, testSuites = tests }) lbi hook bf

preprocess :: BuildInfo -> String -> FilePath -> FilePath -> Verbosity -> IO Bool
preprocess buildInfo tp inFile outFile verbosity = do
  rebuild <- shouldRebuild inFile outFile
  if rebuild
    then do
      notice verbosity $ "[Shuffle] " ++ inFile ++ " -> " ++ outFile
      info verbosity $ "Using the following options:"
      (opts,f,frest) <- getOpts verbosity buildInfo tp [] inFile
      createDirectoryIfMissingVerbose verbosity True (dropFileName outFile)
      out <- openFile outFile WriteMode
      ret <- shuffleCompile out opts f frest
      hClose out
      -- Make sure empty files are actually empty
      when ret $ writeFile outFile ""
      return ret
    else do
      info verbosity $ "[Shuffle] Skipping " ++ inFile
      -- Check filesize to know if file is empty
      size <- withFile outFile ReadMode hFileSize
      return (size == 0)

shouldRebuild :: FilePath -> FilePath -> IO Bool
shouldRebuild inFile outFile = do
  exists <- doesFileExist outFile
  if exists
    then do timeIn <- fpathGetModificationTime (fpathFromStr inFile)
            timeOut <- fpathGetModificationTime (fpathFromStr outFile)
            return $ timeIn > timeOut
    else return True

getOpts :: Verbosity -> BuildInfo -> String -> [String] -> FilePath -> IO (Opts, FPath, [FPathWithAlias])
getOpts verbosity buildInfo tp extra inFile = do
  info verbosity $ unwords ws
  if null errs
    then return (opts, f, frest)
    else die $ unlines errs
  where
    (opts, f, frest, errs) = parseOpts ws
    ws = case ("x-shuffle-" ++ tp) `lookup` customFieldsBI buildInfo of
      Nothing -> extra ++ ["--" ++ tp, inFile]
      Just x  -> argWords x ++ extra ++ ["--" ++ tp, inFile]

-- Similar to words, but don't split on spaces between quotes, i.e.
-- "--test1=1 --test2=\"a b c\"" results in ["--test1=1", "--test2=a b c"]
argWords :: String -> [String]
argWords = map reverse . filter (not . null) . f False ""
  where
    f :: Bool -> String -> String -> [String]
    f _     cur ""       = [cur]
    f True  cur ('"':xs) = f False cur xs
    f True  cur (x:xs)   = f True (x:cur) xs
    f False cur ('"':xs) = f True cur xs
    f False cur (x:xs) | isSpace x = cur : f False "" xs
                       | otherwise = f False (x:cur) xs
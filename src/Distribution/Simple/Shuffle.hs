module Distribution.Simple.Shuffle (shuffleHooks) where

import Distribution.Simple (UserHooks (..))
import Distribution.Simple.PreProcess (PreProcessor (..), mkSimplePreProcessor)
import Distribution.PackageDescription (PackageDescription (..), BuildInfo (..), Executable (..),
                                        Library (..), TestSuite (..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Simple.Utils (dieNoVerbosity, warn, info, notice, findFileWithExtension', 
                                  createDirectoryIfMissingVerbose, getDirectoryContentsRecursive)
import Distribution.Simple.Setup (BuildFlags(..), SDistFlags(..), fromFlagOrDefault)
import Distribution.Verbosity (Verbosity, normal)
import Distribution.ParseUtils (runP, parseOptCommaList, parseFilePathQ, ParseResult (..))
import Distribution.ModuleName (fromString, ModuleName)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)

import Control.Monad (forM, forM_, when)
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.List ((\\), union, intersect, nub, intercalate)
import System.IO (openFile, IOMode(..), hClose, withFile, hFileSize, hGetLine, hIsEOF, hPutStrLn)
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
shuffleHooks h = h { buildHook = shuffleBuildHook (buildHook h)  
                   , sDistHook = mySDist (sDistHook h) }

parseFileList :: String -> String -> Verbosity -> IO [FilePath]
parseFileList fieldName field verbosity =
  case runP 0 fieldName (parseOptCommaList parseFilePathQ) field of
    ParseFailed err    -> dieNoVerbosity $ show err
    ParseOk warnings r -> mapM_ (warn verbosity . show) warnings >> return r

toModuleName :: FilePath -> ModuleName
toModuleName = fromString . map (\x -> if x == pathSeparator then '.' else x) . dropExtension

prepCHS :: [FilePath] -> FilePath -> BuildInfo -> Verbosity -> IO [ModuleName]
prepCHS ignore outDir bi verbosity = do
  fs <- forM (hsSourceDirs bi) $ \dir -> do
    contents <- getDirectoryContentsRecursive dir
    let chs  = filter ((==".chs") . takeExtension) contents
    let chs' = filter (not . (`elem` ignore)) chs
    fs <- forM chs' $ \file -> do
      let outFile = outDir </> replaceExtension file "hs"
      empt <- preprocess bi "hs" (normalise $ dir </> file) outFile verbosity
      return $ if empt then Nothing else Just (toModuleName file)
    return $ catMaybes fs
  return $ concat fs

generateAG :: FilePath -> BuildInfo -> Verbosity -> [String] -> IO [ModuleName]
generateAG outDir bi verbosity files = do
  -- Find all cag files and their dependencies
  deps <- forM files $ \inFile -> do
    mbPath <- findFileWithExtension' [takeExtension inFile] (hsSourceDirs bi) (dropExtension inFile)
    case mbPath of
      Nothing -> dieNoVerbosity $ "can't find source for " ++ inFile ++ " in " ++ intercalate ", " (hsSourceDirs bi)
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
            (_, opts, _, _) <- getOpts bi "dep" ["--depbase=" ++ dir] file
            deps' <- getDeps opts file
            let deps'' = map (\dep -> (dir,replaceExtension dep "cag")) deps'
            return $ (Just modName, deps'')
  -- Preprocess all dependencies
  forM_ (nub $ concat $ map snd deps) $ \(inDir,inFile) -> do
    let outFile = outDir </> replaceExtension inFile "ag"
    preprocess bi "ag" (normalise $ inDir </> inFile) outFile verbosity
  -- Return all extra modules that should be build
  return $ catMaybes $ map fst deps

shuffleBuildHook :: (PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()) -> PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
shuffleBuildHook origBuildHook pd lbi hook bf = do
  let verbosity = fromFlagOrDefault normal (buildVerbosity bf)
  let addOpts :: FilePath -> BuildInfo -> IO ([ModuleName], BuildInfo)
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
        modulesAG <- generateAG outDir bi verbosity allFiles
        -- And preprocess all chs files
        ignore <- case "x-shuffle-hs-ign" `lookup` fields of
          Just files -> parseFileList "x-shuffle-hs-ign" files verbosity
          _          -> return []
        modulesHS <- prepCHS ignore outDir bi verbosity
        -- Update the corresponding fields
        let mods = modulesAG ++ modulesHS
        let newBi = bi { customFieldsBI = dataOpts ++ semOpts ++ semDataOpts ++ customFieldsBI bi
                       , hsSourceDirs = outDir : hsSourceDirs bi }
        return $ (mods, newBi)
  -- Add all options and continue with original hook
  exes <- forM (executables pd) $ \exe -> do
    (mods, newBi) <- addOpts (buildDir lbi </> unUnqualComponentName (exeName exe) </> unUnqualComponentName (exeName exe) ++ "-tmp") (buildInfo exe)
    let newBi' = newBi { otherModules = mods ++ otherModules newBi }
    return $ exe { buildInfo = newBi' }
  lib <- case library pd of
    Just l -> do
      (mods, newBi) <- addOpts (buildDir lbi) (libBuildInfo l)
      return $ Just $ l { libBuildInfo = newBi
                        , exposedModules = mods ++ exposedModules l }
    Nothing -> return Nothing
  tests <- forM (testSuites pd) $ \test -> do
    (mods, newBi) <- addOpts (buildDir lbi </> unUnqualComponentName (testName test) </> unUnqualComponentName (testName test) ++ "-tmp") (testBuildInfo test)
    let newBi' = newBi { otherModules = mods ++ otherModules newBi }
    return $ test { testBuildInfo = newBi' }
  origBuildHook (pd { executables = exes, library = lib, testSuites = tests }) lbi hook bf

preprocess :: BuildInfo -> String -> FilePath -> FilePath -> Verbosity -> IO Bool
preprocess buildInfo tp inFile outFile verbosity = do
  (optstr,opts,f,frest) <- getOpts buildInfo tp [] inFile
  rebuild <- shouldRebuild optstr inFile outFile
  if rebuild
    then do
      notice verbosity $ "[Shuffle] " ++ inFile ++ " -> " ++ outFile
      info verbosity $ "Using the following options: " ++ optstr
      createDirectoryIfMissingVerbose verbosity True (dropFileName outFile)
      out <- openFile outFile WriteMode
      hPutStrLn out $ optline optstr
      empt <- shuffleCompile out opts f frest
      hClose out
      -- Make sure empty files are actually empty
      when empt $ writeFile outFile ""
      return empt
    else do
      info verbosity $ "[Shuffle] Skipping " ++ inFile
      -- Check filesize to know if file is empty
      size <- withFile outFile ReadMode hFileSize
      return (size == 0)

shouldRebuild :: String -> FilePath -> FilePath -> IO Bool
shouldRebuild optstr inFile outFile = do
  exists <- doesFileExist outFile
  if exists
    then do timeIn <- fpathGetModificationTime (fpathFromStr inFile)
            timeOut <- fpathGetModificationTime (fpathFromStr outFile)
            if timeIn > timeOut
              then return True
              else do handle <- openFile outFile ReadMode
                      ans <- do eof <- hIsEOF handle
                                if eof
                                  then return True
                                  else do line <- hGetLine handle
                                          return $ line /= optline optstr
                      hClose handle
                      return ans
    else return True

optline :: String -> String
optline optstr = "-- " ++ optstr

getOpts :: BuildInfo -> String -> [String] -> FilePath -> IO (String, Opts, FPath, [FPathWithAlias])
getOpts buildInfo tp extra inFile = do
  if null errs
    then return (unwords ws, opts, f, frest)
    else dieNoVerbosity $ unlines errs
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

--- For SDist
cagFiles :: BuildInfo -> Verbosity -> [String] -> IO [FilePath]
cagFiles bi verbosity files = do
  -- Find all cag files and their dependencies
  deps <- forM files $ \inFile -> do
    mbPath <- findFileWithExtension' [takeExtension inFile] (hsSourceDirs bi) (dropExtension inFile)
    case mbPath of
      Nothing -> dieNoVerbosity $ "can't find source for " ++ inFile ++ " in " ++ intercalate ", " (hsSourceDirs bi)
      Just (dir,file) -> do
        let f1 = normalise $ dir </> file
        -- Find dependencies
        (_, opts, _, _) <- getOpts bi "dep" ["--depbase=" ++ dir] file
        deps' <- getDeps opts file
        let deps'' = map (\dep -> normalise $ dir </> replaceExtension dep "cag") deps'
        return $ f1 : deps''
  return $ concat deps

chsFiles :: [FilePath] -> BuildInfo -> IO [FilePath]
chsFiles ignore bi = do
  fs <- forM (hsSourceDirs bi) $ \dir -> do
    contents <- getDirectoryContentsRecursive dir
    return $
      map (\file -> normalise $ dir </> file) $ 
      filter (not . (`elem` ignore)) $
      filter ((==".chs") . takeExtension) contents
  return $ concat fs

mySDist :: (PackageDescription -> Maybe LocalBuildInfo -> UserHooks -> SDistFlags -> IO ()) -> PackageDescription -> Maybe LocalBuildInfo -> UserHooks -> SDistFlags -> IO ()
mySDist origSDist pd mblbi hooks flags = do
  let verbosity = fromFlagOrDefault normal (sDistVerbosity flags)
  extraSrc <- mapBuildInfos pd $ \bi -> do
    let fields = customFieldsBI bi
    -- Chs files
    ignore <- case "x-shuffle-hs-ign" `lookup` fields of
          Just files -> parseFileList "x-shuffle-hs-ign" files verbosity
          _          -> return []
    chs <- chsFiles ignore bi
    -- Ag files
    -- Get data files
    dataFiles <- case "x-shuffle-ag-d-dep" `lookup` fields of
          Just files -> parseFileList "x-shuffle-ag-d-dep" files verbosity
          _          -> return []
    -- Get sem files
    semFiles <- case "x-shuffle-ag-s-dep" `lookup` fields of
          Just files -> parseFileList "x-shuffle-ag-s-dep" files verbosity
          _          -> return []
    cag <- cagFiles bi verbosity (dataFiles ++ semFiles)
    return $ chs ++ cag
  let pd' = pd { extraSrcFiles = extraSrcFiles pd ++ concat extraSrc}
  origSDist pd' mblbi hooks flags

mapBuildInfos :: PackageDescription -> (BuildInfo -> IO a) -> IO [a]
mapBuildInfos pd f = do
  exes <- forM (executables pd) (f . buildInfo)
  tests <- forM (testSuites pd) (f . testBuildInfo)
  libs <- case library pd of
    Just lib -> do l <- f (libBuildInfo lib) 
                   return [l]
    Nothing  -> return []
  return $ exes ++ tests ++ libs

{-# LANGUAGE NoImplicitPrelude, LambdaCase #-}

module Main (main) where

import           Conflict (Conflict(..))
import qualified Conflict as Conflict
import qualified Control.Exception as E
import           Control.Monad (when, unless, filterM)
import           Data.Foldable (asum, traverse_)
import           Data.List (isPrefixOf)
import           Data.Maybe (mapMaybe)
import           Environment (checkConflictStyle, openEditor, shouldUseColorByTerminal)
import qualified Opts
import           Opts (Options(..))
import           PPDiff (ppDiff, ColorEnable(..))
import           Resolution (Result(..), NewContent(..), Untabify(..))
import qualified Resolution as Resolution
import           SideDiff (getConflictDiffs, getConflictDiff2s)
import           StrUtils (ensureNewline, stripNewline, unprefix)
import           System.Directory (renameFile, removeFile, getCurrentDirectory)
import           System.Exit (ExitCode(..), exitWith)
import           System.FilePath ((<.>), makeRelative, joinPath, splitPath)
import qualified System.FilePath as FilePath
import           System.IO (hPutStr, stderr)
import qualified System.PosixCompat.Files as PosixFiles
import           System.Process (callProcess, readProcess, readProcessWithExitCode)

import           Prelude.Compat

-- '>' -> ">>>>>>>"
markerPrefix :: Char -> String
markerPrefix = replicate 7

markerLine :: Char -> String -> String
markerLine c str = markerPrefix c ++ " " ++ str ++ "\n"

gitAdd :: FilePath -> IO ()
gitAdd fileName =
    callProcess "git" ["add", "--", fileName]

dumpDiffs :: ColorEnable -> Options -> FilePath -> Int -> (Int, Conflict) -> IO ()
dumpDiffs colorEnable opts filePath count (idx, conflict) =
    do
        putStrLn $ unwords ["### Conflict", show idx, "of", show count]
        when (shouldDumpDiffs opts) $ traverse_ dumpDiff $ getConflictDiffs conflict
        when (shouldDumpDiff2 opts) $ dumpDiff2 $ getConflictDiff2s conflict
    where
        dumpDiff (side, (lineNo, marker), diff) =
            do  putStrLn $ concat
                    [filePath, ":", show lineNo, ":Diff", show side, ": ", marker]
                putStr $ unlines $ map (ppDiff colorEnable) diff
        dumpDiff2 ((lineNoA, markerA), (lineNoB, markerB), diff) =
            do  putStrLn $ concat [filePath, ":", show lineNoA, " <->", markerA]
                putStrLn $ concat [filePath, ":", show lineNoB, ": ", markerB]
                putStr $ unlines $ map (ppDiff colorEnable) diff

dumpAndOpenEditor :: ColorEnable -> Options -> FilePath -> [Conflict] -> IO ()
dumpAndOpenEditor colorEnable opts path conflicts =
    do  when (shouldDumpDiffs opts || shouldDumpDiff2 opts) $
            traverse_ (dumpDiffs colorEnable opts path (length conflicts)) (zip [1..] conflicts)
        openEditor opts path

overwrite :: FilePath -> String -> IO ()
overwrite fileName newContent =
    do  renameFile fileName bkup
        writeFile fileName newContent
        removeFile bkup
    where
        bkup = fileName <.> "bk"

handleFileResult :: ColorEnable -> Options -> FilePath -> NewContent -> IO ()
handleFileResult colorEnable opts fileName (NewContent result newContent)
    | successes == 0 && allGood =
      do  putStrLn $ fileName ++ ": No conflicts, git-adding"
          gitAdd fileName
    | successes == 0 && reductions == 0 =
      do  putStrLn $ concat
              [ fileName, ": Failed to resolve any of the "
              , show failures, " conflicts" ]
          doDump
    | successes == 0 =
      do  putStrLn $ concat
              [ fileName, ": Reduced ", show reductions, " conflicts"]
          overwrite fileName newContent
          doDump
    | otherwise =
      do  putStrLn $ concat
              [ fileName, ": Successfully resolved ", show successes
              , " conflicts (failed to resolve " ++ show (reductions + failures) ++ " conflicts)"
              , if allGood then ", git adding" else ""
              ]
          overwrite fileName newContent
          if allGood
              then gitAdd fileName
              else doDump
    where
        allGood = Resolution.fullySuccessful result
        doDump =
            dumpAndOpenEditor colorEnable opts fileName
            [ conflict | Right conflict <- Conflict.parse newContent ]
        Result
            { _resolvedSuccessfully = successes
            , _reducedConflicts = reductions
            , _failedToResolve = failures
            } = result

resolve :: ColorEnable -> Options -> FilePath -> IO Result
resolve colorEnable opts fileName =
    do
        resolutions <-
            Resolution.resolveContent (Untabify (Opts.untabify opts))
            . Conflict.parse
            <$> readFile fileName
        _result resolutions <$ handleFileResult colorEnable opts fileName resolutions

relativePath :: FilePath -> FilePath -> FilePath
relativePath base path
    | rel /= path = rel
    | revRel /= base =
          joinPath $ replicate (length (splitPath revRel)) ".."
    | otherwise = path
    where
        rel = makeRelative base path
        revRel = makeRelative path base

(</>) :: FilePath -> FilePath -> FilePath
"." </> p = p
d </> p = d FilePath.</> p

isDirectory :: FilePath -> IO Bool
isDirectory x = PosixFiles.isDirectory <$> PosixFiles.getFileStatus x

withAllStageFiles ::
    FilePath -> (FilePath -> Maybe FilePath -> Maybe FilePath -> IO b) -> IO b
withAllStageFiles path action =
    do  let stdin = ""
        [baseTmpRaw, localTmpRaw, remoteTmpRaw] <-
            take 3 . words <$>
            readProcess "git" ["checkout-index", "--stage=all", "--", path] stdin
        cdup <-
            takeWhile (/= '\0') . stripNewline <$>
            readProcess "git" ["rev-parse", "--show-cdup"] stdin
        let maybePath "." = Nothing
            maybePath p = Just (cdup </> p)
        let mLocalTmp = maybePath localTmpRaw
            mRemoteTmp = maybePath remoteTmpRaw
            baseTmp = cdup </> baseTmpRaw
        action baseTmp mLocalTmp mRemoteTmp
            `E.finally`
            do  removeFile baseTmp
                traverse_ removeFile mLocalTmp
                traverse_ removeFile mRemoteTmp

deleteModifyConflictAddMarkers :: FilePath -> IO ()
deleteModifyConflictAddMarkers path =
    withAllStageFiles path $ \baseTmp mLocalTmp mRemoteTmp ->
    do  baseContent <- readFile baseTmp
        localContent <- maybe (return "") readFile mLocalTmp
        remoteContent <- maybe (return "") readFile mRemoteTmp
        overwrite path $
            concat
            [ markerLine '<' "LOCAL"
            , ensureNewline localContent
            , markerLine '|' "BASE"
            , ensureNewline baseContent
            , markerLine '=' ""
            , ensureNewline remoteContent
            , markerLine '>' "REMOTE"
            ]

deleteModifyConflictHandle :: FilePath -> IO ()
deleteModifyConflictHandle path =
    do  marked <-
            any (markerPrefix '<' `isPrefixOf`) . lines <$> readFile path
        unless marked $
            do  putStrLn $ show path ++ " has a delete/modify conflict. Adding conflict markers"
                deleteModifyConflictAddMarkers path

removeFileIfEmpty :: FilePath -> IO ()
removeFileIfEmpty path =
    do  isEmpty <- null <$> readFile path
        when isEmpty $
            do  removeFile path
                callProcess "git" ["add", "-u", "--", path]

getStatusPorcelain :: IO String
getStatusPorcelain =
    do  (statusCode, statusPorcelain, statusStderr) <-
            readProcessWithExitCode "git" ["status", "--porcelain"] ""
        when (statusCode /= ExitSuccess) $ do
            -- Print git's error message. Usually -
            -- "fatal: Not a git repository (or any of the parent directories): .git"
            hPutStr stderr statusStderr
            exitWith statusCode
        return statusPorcelain

getGitRootDir :: IO FilePath
getGitRootDir =
  do  cwd <- getCurrentDirectory
      relativePath cwd . stripNewline <$>
          readProcess "git" ["rev-parse", "--show-toplevel"] ""

makeFilesMatchingPrefixes :: IO ([String] -> IO [FilePath])
makeFilesMatchingPrefixes =
  do  statusPorcelain <- getStatusPorcelain
      rootDir <- getGitRootDir
      let rootRelativeFiles =
              filterM (fmap not . isDirectory) . map (rootDir </>)
      let firstMatchingPrefix :: [String] -> String -> Maybe String
          firstMatchingPrefix prefixes =
              asum . traverse unprefix prefixes
      let filesMatchingPrefixes :: [String] -> IO [FilePath]
          filesMatchingPrefixes prefixes =
              rootRelativeFiles . mapMaybe (firstMatchingPrefix prefixes)
              $ lines statusPorcelain
      pure filesMatchingPrefixes

mediateAll :: ColorEnable -> Options -> IO Result
mediateAll colorEnable opts =
  do  filesMatchingPrefixes <- makeFilesMatchingPrefixes

-- from git-diff manpage:
-- Added (A), Copied (C), Deleted (D), Modified (M), Renamed (R),
-- have their type (i.e. regular file, symlink, submodule, ...) changed (T),
-- are Unmerged (U), are Unknown (X), or have had their pairing Broken (B)

      deleteModifyConflicts <- filesMatchingPrefixes ["DU ", "UD "]

      traverse_ deleteModifyConflictHandle deleteModifyConflicts

      result <- filesMatchingPrefixes ["UU ", "AA ", "DA ", "AD ", "DU ", "UD "]
          >>= foldMap (resolve colorEnable opts)

      -- Heuristically delete files that were remove/modify conflict
      -- and ended up with empty content
      traverse_ removeFileIfEmpty deleteModifyConflicts
      pure result

exitCodeOf :: Result -> ExitCode
exitCodeOf result
    | Resolution.fullySuccessful result = ExitSuccess
    | otherwise = ExitFailure 111

exitProcess :: Result -> IO ()
exitProcess = exitWith . exitCodeOf

main :: IO ()
main =
  do  opts <- Opts.getOpts
      colorEnable <-
          case shouldUseColor opts of
              Nothing -> shouldUseColorByTerminal
              Just colorEnable -> return colorEnable
      checkConflictStyle opts
      case mergeSpecificFile opts of
          Nothing -> mediateAll colorEnable opts
          Just path -> resolve colorEnable opts path
          >>= exitProcess

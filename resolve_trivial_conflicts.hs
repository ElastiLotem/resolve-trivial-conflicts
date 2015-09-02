{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, RecordWildCards #-}
module Main (main) where

import qualified Control.Exception as E
import           Control.Monad (when, unless, filterM)
import           Control.Monad.State (MonadState, state, evalStateT)
import           Control.Monad.Writer (runWriter, tell)
import           Data.Algorithm.Diff (Diff, getDiff)
import           Data.Foldable (asum, traverse_)
import           Data.List (isPrefixOf, isSuffixOf)
import           Data.Maybe (mapMaybe)
import qualified Data.Monoid as Monoid
import           PPDiff (ppDiff, ColorEnable(..))
import           System.Directory (renameFile, removeFile, getCurrentDirectory)
import           System.Environment (getProgName, getArgs, getEnv)
import           System.Exit (ExitCode(..))
import           System.FilePath ((<.>), makeRelative, joinPath, splitPath)
import qualified System.FilePath as FilePath
import qualified System.Posix.Files as PosixFiles
import           System.Posix.IO (stdOutput)
import           System.Posix.Terminal (queryTerminal)
import           System.Process (callProcess, readProcess, readProcessWithExitCode)

import           Prelude.Compat

data Side = A | B
  deriving (Eq, Ord, Show)
type LineNo = Int

data Conflict = Conflict
  { cMarkerA    :: (LineNo, String) -- <<<<<<<....
  , cMarkerBase :: (LineNo, String) -- |||||||....
  , cMarkerB    :: (LineNo, String) -- =======....
  , cMarkerEnd  :: (LineNo, String) -- >>>>>>>....
  , cLinesA     :: [String]
  , cLinesBase  :: [String]
  , cLinesB     :: [String]
  } deriving (Show)

prettyConflict :: Conflict -> String
prettyConflict Conflict {..} =
  unlines $ concat
  [ snd cMarkerA    : cLinesA
  , snd cMarkerBase : cLinesBase
  , snd cMarkerB    : cLinesB
  , [snd cMarkerEnd]
  ]

resolveConflict :: Conflict -> Maybe String
resolveConflict Conflict{..}
  | cLinesA == cLinesBase = Just $ unlines cLinesB
  | cLinesB == cLinesBase = Just $ unlines cLinesA
  | cLinesA == cLinesB = Just $ unlines cLinesA
  | otherwise = Nothing

-- '>' -> ">>>>>>> "
markerPrefix :: Char -> String
markerPrefix c = replicate 7 c

markerLine :: Char -> String -> String
markerLine c str = markerPrefix c ++ " " ++ str ++ "\n"

breakUpToMarker :: MonadState [(LineNo, String)] m => Char -> m [(LineNo, String)]
breakUpToMarker c = state (break ((markerPrefix c `isPrefixOf`) . snd))

readHead :: MonadState [a] m => m (Maybe a)
readHead = state f
  where
    f [] = (Nothing, [])
    f (l:ls) = (Just l, ls)

tryReadUpToMarker :: MonadState [(LineNo, String)] m => Char -> m ([(LineNo, String)], Maybe (LineNo, String))
tryReadUpToMarker c =
  do
    ls <- breakUpToMarker c
    mHead <- readHead
    return (ls, mHead)

readUpToMarker :: MonadState [(LineNo, String)] m => Char -> m ([(LineNo, String)], (LineNo, String))
readUpToMarker c = do
  res <- tryReadUpToMarker c
  case res of
    (ls, Just h)  -> return $ (ls, h)
    (ls, Nothing) ->
      error $ concat
      [ "Parse error: failed reading up to marker: "
      , show c, ", got:"
      , concatMap (\(l,s) -> "\n" ++ show l ++ "\t" ++ s) $ take 5 ls
      ]

parseConflict :: MonadState [(LineNo, String)] m => (LineNo, String) -> m Conflict
parseConflict markerA = do
  (linesA   , markerBase) <- readUpToMarker '|'
  (linesBase, markerB)    <- readUpToMarker '='
  (linesB   , markerEnd)  <- readUpToMarker '>'
  return Conflict
    { cMarkerA    = markerA
    , cMarkerBase = markerBase
    , cMarkerB    = markerB
    , cMarkerEnd  = markerEnd
    , cLinesA     = map snd linesA
    , cLinesB     = map snd linesB
    , cLinesBase  = map snd linesBase
    }

parseConflicts :: String -> [Either String Conflict]
parseConflicts input =
  snd $ runWriter $ evalStateT loop (zip [1..] (lines input))
  where
    loop =
      do
        (ls, mMarkerA) <- tryReadUpToMarker '<'
        tell $ map (Left . snd) ls
        case mMarkerA of
          Nothing -> return ()
          Just markerA ->
            do
              tell . return . Right =<< parseConflict markerA
              loop

type SideDiff = (Side, (LineNo, String), [Diff String])

data NewContent = NewContent
  { _resolvedSuccessfully :: Int
  , _failedToResolve :: Int
  , _newContent :: String
  }

getConflictDiffs :: Conflict -> [SideDiff]
getConflictDiffs Conflict{..} =
    [ (A, cMarkerA, getDiff cLinesBase cLinesA) | not (null cLinesA) ] ++
    [ (B, (fst cMarkerB, snd cMarkerEnd), getDiff cLinesBase cLinesB) | not (null cLinesB) ]

resolveContent :: [Either String Conflict] -> NewContent
resolveContent = asResult . mconcat . map go
  where
    asResult (Monoid.Sum successes, Monoid.Sum failures, newContent) =
      NewContent
      { _resolvedSuccessfully = successes
      , _failedToResolve = failures
      , _newContent = newContent
      }
    go (Left line) = (Monoid.Sum 0, Monoid.Sum 0, unlines [line])
    go (Right conflict) =
      case resolveConflict conflict of
      Nothing -> (Monoid.Sum 0, Monoid.Sum 1, prettyConflict conflict)
      Just trivialLines -> (Monoid.Sum 1, Monoid.Sum 0, trivialLines)

gitAdd :: FilePath -> IO ()
gitAdd fileName =
  callProcess "git" ["add", "--", fileName]

data Options = Options
  { shouldUseEditor :: Bool
  , shouldDumpDiffs :: Bool
  , shouldUseColor :: Maybe ColorEnable
  , shouldSetConflictStyle :: Bool
  }
instance Monoid Options where
  mempty = Options False False Nothing False
  Options oe0 od0 oc0 os0 `mappend` Options oe1 od1 oc1 os1 =
    Options
    (combineBool oe0 oe1 "-e")
    (combineBool od0 od1 "-d")
    (combineMaybe oc0 oc1 "-c or -C")
    (os0 || os1)
    where
      err flag = error $ "Multiple " ++ flag ++ " flags used"
      combineMaybe (Just _) (Just _) flag = err flag
      combineMaybe Nothing Nothing _ = Nothing
      combineMaybe (Just x) Nothing _ = Just x
      combineMaybe Nothing (Just y) _ = Just y
      combineBool True True flag = err flag
      combineBool x y _ = x || y

getOpts :: [String] -> IO Options
getOpts = fmap mconcat . mapM parseArg
  where
    parseArg "-e" = return mempty { shouldUseEditor = True }
    parseArg "-d" = return mempty { shouldDumpDiffs = True }
    parseArg "-c" = return mempty { shouldUseColor = Just EnableColor }
    parseArg "-C" = return mempty { shouldUseColor = Just DisableColor }
    parseArg "-s" = return mempty { shouldSetConflictStyle = True }
    parseArg arg =
      do  prog <- getProgName
          putStr $ unlines
            [ "Usage: " ++ prog ++ " [-e] [-d] [-c] [-C] [-s]"
            , ""
            , "-e    Execute $EDITOR for each conflicted file that remains conflicted"
            , "-d    Dump the left/right diffs from base in each conflict remaining"
            , "-c    Enable color"
            , "-C    Disable color"
            , "-s    Configure git's global merge.conflictstyle to diff3 if needed"
            ]
          fail $ "Unknown argument: " ++ show arg

openEditor :: Options -> FilePath -> IO ()
openEditor opts path
  | shouldUseEditor opts =
    do
      editor <- getEnv "EDITOR"
      callProcess editor [path]
  | otherwise = return ()

dumpDiffs :: ColorEnable -> Options -> FilePath -> [SideDiff] -> IO ()
dumpDiffs colorEnable opts filePath diffs
  | shouldDumpDiffs opts = mapM_ dumpDiff diffs
  | otherwise = return ()
  where
    dumpDiff (side, (lineNo, marker), diff) =
      do
        putStrLn $ concat
            [filePath, ":", show lineNo, ":Diff", show side, ": ", marker]
        putStr $ unlines $ map (ppDiff colorEnable) diff

dumpAndOpenEditor :: ColorEnable -> Options -> FilePath -> [SideDiff] -> IO ()
dumpAndOpenEditor colorEnable opts path diffs =
  do
    dumpDiffs colorEnable opts path diffs
    openEditor opts path

overwrite :: FilePath -> String -> IO ()
overwrite fileName newContent =
    do
        renameFile fileName bkup
        writeFile fileName newContent
        removeFile bkup
    where
        bkup = fileName <.> "bk"

resolve :: ColorEnable -> Options -> FilePath -> IO ()
resolve colorEnable opts fileName =
  do
    content <- parseConflicts <$> readFile fileName
    case resolveContent content of
      NewContent successes failures newContent
        | successes == 0 &&
          failures == 0 -> do
            putStrLn $ fileName ++ ": No conflicts, git-adding"
            gitAdd fileName
        | successes == 0 -> do
            putStrLn $ concat
              [ fileName, ": Failed to resolve any of the "
              , show failures, " conflicts" ]
            doDump
        | otherwise ->
          do
            putStrLn $ concat
              [ fileName, ": Successfully resolved ", show successes
              , " conflicts (failed to resolve " ++ show failures ++ " conflicts)"
              , if failures == 0 then ", git adding" else ""
              ]
            overwrite fileName newContent
            if failures == 0
              then gitAdd fileName
              else doDump
        where
          doDump =
            dumpAndOpenEditor colorEnable opts fileName
                [ cDiff
                | Right conflict <- parseConflicts newContent
                , cDiff <- getConflictDiffs conflict
                ]

stripNewline :: String -> String
stripNewline x
    | "\n" `isSuffixOf` x = init x
    | otherwise = x

shouldUseColorByTerminal :: IO ColorEnable
shouldUseColorByTerminal =
    do  istty <- queryTerminal stdOutput
        return $ if istty then EnableColor else DisableColor

unprefix :: Eq a => [a] -> [a] -> Maybe [a]
unprefix prefix str
    | prefix `isPrefixOf` str = Just (drop (length prefix) str)
    | otherwise = Nothing

getConflictStyle :: IO String
getConflictStyle =
    do  (exitCode, stdout, _) <- readProcessWithExitCode "git" ["config", "merge.conflictstyle"] stdin
        case exitCode of
            ExitSuccess -> return $ stripNewline stdout
            ExitFailure 1 -> return "unset"
            ExitFailure _ -> E.throwIO exitCode
    where
        stdin = ""

setConflictStyle :: IO ()
setConflictStyle =
    callProcess "git" ["config", "--global", "merge.conflictstyle", "diff3"]

checkConflictStyle :: Options -> IO ()
checkConflictStyle opts =
    do  conflictStyle <- getConflictStyle
        when (conflictStyle /= "diff3") $
            do  unless (shouldSetConflictStyle opts) $
                    fail $ concat
                    [ "merge.conflictstyle must be diff3 but is "
                    , show conflictStyle
                    , ". Use -s to automatically set it globally"
                    ]
                setConflictStyle

                newConflictStyle <- getConflictStyle
                when (newConflictStyle /= "diff3") $
                    fail $ concat
                    [ "Attempt to set conflict style failed. Perhaps you have"
                    , " an incorrect merge.conflictstyle configuration "
                    , "specified in your per-project .git/config?"
                    ]

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

ensureNewline :: String -> String
ensureNewline "" = ""
ensureNewline str = str ++ suffix
    where
        suffix
            | "\n" `isSuffixOf` str = ""
            | otherwise = "\n"

withAllStageFiles ::
    FilePath -> (FilePath -> Maybe FilePath -> Maybe FilePath -> IO b) -> IO b
withAllStageFiles path action =
    do
        let stdin = ""
        [baseTmp, localTmp, remoteTmp] <-
            take 3 . words <$>
            readProcess "git" ["checkout-index", "--stage=all", "--", path] stdin
        let maybePath "." = Nothing
            maybePath p = Just p
        let mLocalTmp = maybePath localTmp
            mRemoteTmp = maybePath remoteTmp
        action baseTmp mLocalTmp mRemoteTmp
            `E.finally`
            do
                removeFile baseTmp
                traverse_ removeFile mLocalTmp
                traverse_ removeFile mRemoteTmp

deleteModifyConflictAddMarkers :: FilePath -> IO ()
deleteModifyConflictAddMarkers path =
    withAllStageFiles path $ \baseTmp mLocalTmp mRemoteTmp ->
    do
        baseContent <- readFile baseTmp
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
    do  notMarked <- null . filter (markerPrefix '<' `isPrefixOf`) . lines <$> readFile path
        when notMarked $
            do  putStrLn $ show path ++ " has a delete/modify conflict. Adding conflict markers"
                deleteModifyConflictAddMarkers path

removeFileIfEmpty :: FilePath -> IO ()
removeFileIfEmpty path =
    do  isEmpty <- null <$> readFile path
        when isEmpty $
            do  removeFile path
                callProcess "git" ["add", "-u", "--", path]

main :: IO ()
main =
  do  opts <- getOpts =<< getArgs
      colorEnable <-
          case shouldUseColor opts of
              Nothing -> shouldUseColorByTerminal
              Just colorEnable -> return colorEnable
      checkConflictStyle opts
      let stdin = ""
      statusPorcelain <- readProcess "git" ["status", "--porcelain"] stdin
      cwd <- getCurrentDirectory
      rootDir <-
          relativePath cwd . stripNewline <$>
          readProcess "git" ["rev-parse", "--show-toplevel"] stdin
      let rootRelativeFiles =
              filterM (fmap not . isDirectory) . map (rootDir </>)
      let firstMatchingPrefix :: [String] -> String -> Maybe String
          firstMatchingPrefix prefixes =
              asum . traverse unprefix prefixes
      let filesMatchingPrefixes :: [String] -> IO [FilePath]
          filesMatchingPrefixes prefixes =
              rootRelativeFiles . mapMaybe (firstMatchingPrefix prefixes)
              $ lines statusPorcelain

-- from git-diff manpage:
-- Added (A), Copied (C), Deleted (D), Modified (M), Renamed (R),
-- have their type (i.e. regular file, symlink, submodule, ...) changed (T),
-- are Unmerged (U), are Unknown (X), or have had their pairing Broken (B)

      deleteModifyConflicts <- filesMatchingPrefixes ["DU ", "UD "]

      mapM_ deleteModifyConflictHandle deleteModifyConflicts

      filesMatchingPrefixes ["UU ", "AA ", "DA ", "AD ", "DU ", "UD "]
          >>= mapM_ (resolve colorEnable opts)

      -- Heuristically delete files that were remove/modify conflict
      -- and ended up with empty content
      mapM_ removeFileIfEmpty deleteModifyConflicts

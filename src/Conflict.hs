{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, RecordWildCards #-}

module Conflict
    ( Conflict(..), LineNo
    , bodyStrings, setBodyStrings
    , pretty, prettyLines
    , parse
    ) where

import Control.Monad.State (MonadState, state, evalStateT)
import Control.Monad.Writer (runWriter, tell)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)

import Prelude.Compat

type LineNo = Int

data Conflict = Conflict
    { cMarkerA    :: (LineNo, String) -- <<<<<<<....
    , cMarkerBase :: (LineNo, String) -- |||||||....
    , cMarkerB    :: (LineNo, String) -- =======....
    , cMarkerEnd  :: (LineNo, String) -- >>>>>>>....
    , cBodyA      :: [String]
    , cBodyBase   :: [Either String Conflict]
    , cBodyB      :: [String]
    } deriving (Show)

-- traversal
bodyStrings :: Applicative f => (String -> f String) -> Conflict -> f Conflict
bodyStrings f c@Conflict{..} =
    mk <$> traverse f cBodyA <*> traverse onBase cBodyBase <*> traverse f cBodyB
    where
        mk bodyA bodyBase bodyB =
            c{cBodyA=bodyA, cBodyBase=bodyBase, cBodyB=bodyB}
        onBase (Left x) = Left <$> f x
        onBase (Right x) = Right <$> bodyStrings f x

-- setter:
setBodyStrings :: (String -> String) -> Conflict -> Conflict
setBodyStrings f = runIdentity . bodyStrings (Identity . f)

prettyLines :: Conflict -> [String]
prettyLines Conflict {..} =
    concat
    [ snd cMarkerA    : cBodyA
    , snd cMarkerBase : (cBodyBase >>= either pure prettyLines)
    , snd cMarkerB    : cBodyB
    , [snd cMarkerEnd]
    ]

pretty :: Conflict -> String
pretty = unlines . prettyLines

breakUpToMarker ::
    MonadState [(LineNo, String)] m =>
    Char -> Maybe Int -> m [(LineNo, String)]
breakUpToMarker c mCount =
    state (break cond)
    where
        count = fromMaybe 7 mCount
        prefix = replicate count c
        cond (_, line) =
            pre == prefix && rightCount
            where
                (pre, post) = splitAt count line
                rightCount =
                    case (mCount, post) of
                    (Just{}, (x:_)) -> c /= x
                    _ -> True

readHead :: MonadState [a] m => m (Maybe a)
readHead = state f
    where
        f [] = (Nothing, [])
        f (l:ls) = (Just l, ls)

tryReadUpToMarker ::
    MonadState [(LineNo, String)] m =>
    Char -> Maybe Int -> m ([(LineNo, String)], Maybe (LineNo, String))
tryReadUpToMarker c mCount =
    do
        ls <- breakUpToMarker c mCount
        mHead <- readHead
        return (ls, mHead)

readUpToMarker ::
    MonadState [(LineNo, String)] m =>
    Char -> Maybe Int -> m ([(LineNo, String)], (LineNo, String))
readUpToMarker c mCount = do
    res <- tryReadUpToMarker c mCount
    case res of
        (ls, Just h)  -> return (ls, h)
        (ls, Nothing) ->
            error $ concat
            [ "Parse error: failed reading up to marker: "
            , show c, ", got:"
            , concatMap (\(l,s) -> "\n" ++ show l ++ "\t" ++ s) $ take 5 ls
            ]

parseConflict :: MonadState [(LineNo, String)] m => (LineNo, String) -> m Conflict
parseConflict markerA =
    do  (linesA   , markerBase) <- readUpToMarker '|' markerCount
        (linesBase, markerB)    <- readUpToMarker '=' markerCount
        (linesB   , markerEnd)  <- readUpToMarker '>' markerCount
        return Conflict
            { cMarkerA    = markerA
            , cMarkerBase = markerBase
            , cMarkerB    = markerB
            , cMarkerEnd  = markerEnd
            , cBodyA      = map snd linesA
            , cBodyBase   = parseFromNumberedLines linesBase
            , cBodyB      = map snd linesB
            }
    where
        markerCount = Just (length (takeWhile (== '<') (snd markerA)))

parseFromNumberedLines :: [(LineNo, String)] -> [Either String Conflict]
parseFromNumberedLines =
    snd . runWriter . evalStateT loop
    where
        loop =
            do  (ls, mMarkerA) <- tryReadUpToMarker '<' Nothing
                tell $ map (Left . snd) ls
                case mMarkerA of
                    Nothing -> return ()
                    Just markerA ->
                        do  tell . return . Right =<< parseConflict markerA
                            loop

parse :: String -> [Either String Conflict]
parse input =
    parseFromNumberedLines (zip [1..] (lines input))

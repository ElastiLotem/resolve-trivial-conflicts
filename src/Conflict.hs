{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, DeriveTraversable, NamedFieldPuns, DerivingVia, DeriveGeneric #-}

module Conflict
    ( Conflict(..), Sides(..), LineNo
    , bodies, setBodies
    , pretty, prettyLines
    , parse
    , markerPrefix
    , Tactic(..), getTactic
    ) where

import Control.Monad.State (MonadState, state, evalStateT)
import Control.Monad.Writer (runWriter, tell)
import Data.Functor.Identity (Identity(..))
import Data.List (isPrefixOf)
import Generic.Data (Generically(..), Generically1(..))
import GHC.Generics (Generic, Generic1)
import Text.Read (readMaybe)

import Prelude.Compat

type LineNo = Int

data Sides a = Sides
    { sideA :: a
    , sideBase :: a
    , sideB :: a
    } deriving (Functor, Foldable, Traversable, Show, Eq, Ord, Generic, Generic1)
    deriving (Semigroup, Monoid) via Generically (Sides a)
    deriving Applicative via Generically1 Sides

data Conflict = Conflict
    { cMarkers   :: Sides (LineNo, String) -- The markers at the beginning of sections
    , cMarkerEnd :: (LineNo, String)       -- The ">>>>>>>...." marker at the end of the conflict
    , cBodies    :: Sides [String]
    } deriving (Show)

newtype Tactic = Grow Int deriving (Read, Show, Eq)

getTactic :: Conflict -> Maybe Tactic
getTactic =
    readMaybe . dropWhile (== '=') . snd . sideB . cMarkers

-- traversal
bodies :: Applicative f => (Sides [String] -> f (Sides [String])) -> Conflict -> f Conflict
bodies f c@Conflict{cBodies} = (\x -> c{cBodies = x}) <$> f cBodies

-- setter:
setBodies :: (Sides [String] -> Sides [String]) -> Conflict -> Conflict
setBodies f = runIdentity . bodies (Identity . f)

prettyLines :: Conflict -> [String]
prettyLines Conflict{cMarkers, cMarkerEnd, cBodies} =
    concat ((:) <$> (snd <$> cMarkers) <*> cBodies) <> [snd cMarkerEnd]

pretty :: Conflict -> String
pretty = unlines . prettyLines

-- '>' -> ">>>>>>>"
markerPrefix :: Char -> String
markerPrefix = replicate 7

breakUpToMarker :: MonadState [(LineNo, String)] m => Char -> m [(LineNo, String)]
breakUpToMarker c = state (break ((markerPrefix c `isPrefixOf`) . snd))

readHead :: MonadState [a] m => m (Maybe a)
readHead = state f
    where
        f [] = (Nothing, [])
        f (l:ls) = (Just l, ls)

tryReadUpToMarker :: MonadState [(LineNo, String)] m => Char -> m ([(LineNo, String)], Maybe (LineNo, String))
tryReadUpToMarker c =
    (,) <$> breakUpToMarker c <*> readHead

readUpToMarker :: MonadState [(LineNo, String)] m => Char -> m ([(LineNo, String)], (LineNo, String))
readUpToMarker c = do
    res <- tryReadUpToMarker c
    case res of
        (ls, Just h)  -> pure (ls, h)
        (ls, Nothing) ->
            error $ concat
            [ "Parse error: failed reading up to marker: "
            , show c, ", got:"
            , concatMap (\(l,s) -> "\n" ++ show l ++ "\t" ++ s) $ take 5 ls
            ]

parseConflict :: MonadState [(LineNo, String)] m => (LineNo, String) -> m Conflict
parseConflict markerA =
    do  (linesA   , markerBase) <- readUpToMarker '|'
        (linesBase, markerB)    <- readUpToMarker '='
        (linesB   , markerEnd)  <- readUpToMarker '>'
        pure Conflict
            { cMarkers    = Sides markerA markerBase markerB
            , cMarkerEnd  = markerEnd
            , cBodies     = fmap snd <$> Sides linesA linesBase linesB
            }

parseFromNumberedLines :: [(LineNo, String)] -> [Either String Conflict]
parseFromNumberedLines =
    snd . runWriter . evalStateT loop
    where
        loop =
            do  (ls, mMarkerA) <- tryReadUpToMarker '<'
                tell $ map (Left . snd) ls
                case mMarkerA of
                    Nothing -> pure ()
                    Just markerA ->
                        do  tell . pure . Right =<< parseConflict markerA
                            loop

parse :: String -> [Either String Conflict]
parse = parseFromNumberedLines . zip [1..] . lines

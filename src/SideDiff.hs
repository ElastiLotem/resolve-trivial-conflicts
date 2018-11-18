{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

module SideDiff
    ( SideDiff, Side(..)
    , getConflictDiffs, getConflictDiff2s
    ) where

import           Conflict (Conflict(..), LineNo)
import           Data.Algorithm.Diff (Diff, getDiff)
import           Data.Either (partitionEithers)

import           Prelude.Compat

data Side = A | B
    deriving (Eq, Ord, Show)

type SideDiff = (Side, (LineNo, String), [Diff String])

getConflictDiffs :: Conflict -> [SideDiff]
getConflictDiffs Conflict{..} =
    case partitionEithers cBodyBase of
    (baseLines, []) ->
        [ (A, cMarkerA, getDiff baseLines cBodyA)
        | not (null cBodyA) ] ++
        [ (B, (fst cMarkerB, snd cMarkerEnd), getDiff baseLines cBodyB)
        | not (null cBodyB) ]
    (_, nested) -> nested >>= getConflictDiffs

getConflictDiff2s :: Conflict -> ((LineNo, String), (LineNo, String), [Diff String])
getConflictDiff2s Conflict{..} = (cMarkerA, cMarkerB, getDiff cBodyA cBodyB)

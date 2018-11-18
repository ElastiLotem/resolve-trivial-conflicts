{-# LANGUAGE NoImplicitPrelude, RecordWildCards, BangPatterns #-}

module Resolution
    ( Result(..)
    , NewContent(..)
    , Untabify(..)
    , resolveContent
    , fullySuccessful
    ) where

import           Conflict (Conflict(..))
import qualified Conflict as Conflict
import           Data.Either (partitionEithers)
import qualified Data.Monoid as Monoid

import           Prelude.Compat

data Resolution
    = NoResolution
    | Resolution String
    | PartialResolution String

resolveSimpleConflict :: Conflict -> [String] -> Resolution
resolveSimpleConflict conflict@Conflict{..} baseLines
    | cBodyA == baseLines = Resolution $ unlines cBodyB
    | cBodyB == baseLines = Resolution $ unlines cBodyA
    | cBodyA == cBodyB = Resolution $ unlines cBodyA
    | matchTop > 0 || matchBottom > 0 =
        PartialResolution $ unlines $
        take matchTop cBodyA ++
        Conflict.prettyLines conflict
        { cBodyA = unmatched cBodyA
        , cBodyBase = map Left (unmatched baseLines)
        , cBodyB = unmatched cBodyB
        } ++
        takeEnd matchBottom cBodyA
    | otherwise = NoResolution
    where
        match base a b
            | null base = lengthOfCommonPrefix a b
            | otherwise = minimum $ map (lengthOfCommonPrefix base) [a, b]
        matchTop = match baseLines cBodyA cBodyB
        revBottom = reverse . drop matchTop
        matchBottom = match (revBottom baseLines) (revBottom cBodyA) (revBottom cBodyB)
        dropEnd count xs = take (length xs - count) xs
        takeEnd count xs = drop (length xs - count) xs
        unmatched xs = drop matchTop $ dropEnd matchBottom xs

resolveConflict :: Conflict -> Resolution
resolveConflict conflict@Conflict{..} =
    case partitionEithers resolvedBase of
    (baseLines, []) -> resolveSimpleConflict conflict baseLines
    (_, resolutions) | all (isNoResolution . fst) resolutions -> NoResolution
    _ ->
        PartialResolution $
        Conflict.pretty conflict
        { cBodyBase = map (Left . either id (Conflict.pretty . snd)) resolvedBase
        }
    where
        resolvedBase = (fmap . fmap) resolveBaseConflict cBodyBase
        resolveBaseConflict x = (resolveConflict x, x)
        isNoResolution NoResolution = True
        isNoResolution _ = False

lengthOfCommonPrefix :: Eq a => [a] -> [a] -> Int
lengthOfCommonPrefix x y = length $ takeWhile id $ zipWith (==) x y

data Result = Result
    { _resolvedSuccessfully :: !Int
    , _reducedConflicts :: !Int
    , _failedToResolve :: !Int
    }

fullySuccessful :: Result -> Bool
fullySuccessful (Result _ reduced failed) = reduced == 0 && failed == 0

instance Semigroup Result where
    Result x0 y0 z0 <> Result x1 y1 z1 = Result (x0+x1) (y0+y1) (z0+z1)

instance Monoid Result where mempty = Result 0 0 0

data NewContent = NewContent
    { _result :: !Result
    , _newContent :: !String
    }

instance Semigroup NewContent where
    NewContent x0 y0 <> NewContent x1 y1 = NewContent (x0<>x1) (y0<>y1)

instance Monoid NewContent where mempty = NewContent mempty mempty

newtype Untabify = Untabify { mUntabifySize :: Maybe Int }

untabifyStr :: Int -> String -> String
untabifyStr size =
    go 0
    where
        cyclicInc col
            | col >= size - 1 = 0
            | otherwise = col + 1
        go !col ('\t':rest) = replicate (size - col) ' ' ++ go 0 rest
        go !col (x:rest) = x : go (cyclicInc col) rest
        go _ [] = []

untabify :: Int -> Conflict -> Conflict
untabify = Conflict.setBodyStrings . untabifyStr

resolveContent :: Untabify -> [Either String Conflict] -> NewContent
resolveContent (Untabify mUntabifySize) =
    foldMap go
    where
        untabified = maybe id untabify mUntabifySize
        go (Left line) = NewContent mempty (unlines [line])
        go (Right conflict) =
            case resolveConflict (untabified conflict) of
            NoResolution               -> NewContent (Result 0 0 1)
                                          (Conflict.pretty (untabified conflict))
            Resolution trivialLines    -> NewContent (Result 1 0 0) trivialLines
            PartialResolution newLines -> NewContent (Result 0 1 0) newLines

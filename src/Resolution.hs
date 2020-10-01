{-# LANGUAGE NoImplicitPrelude, BangPatterns, NamedFieldPuns #-}

module Resolution
    ( Result(..)
    , NewContent(..)
    , Untabify(..)
    , resolveContent
    , fullySuccessful
    ) where

import           Conflict (Conflict(..), Sides(..))
import qualified Conflict
import           Control.Arrow (Arrow(first))
import           Data.Foldable (Foldable(..))

import           Prelude.Compat

data Resolution
    = NoResolution
    | Resolution String
    | PartialResolution String

resolveGen :: Eq a => Sides a -> Maybe a
resolveGen (Sides a base b)
    | a == base = Just b
    | b == base = Just a
    | a == b = Just a
    | otherwise = Nothing

resolveConflict :: Conflict -> Resolution
resolveConflict conflict@Conflict {cBodies} =
    case resolveGen cBodies of
    Just r -> Resolution $ unlines r
    Nothing
        | Conflict.getTactic conflict /= Just (Conflict.Grow 0) && (matchTop > 0 || matchBottom > 0) ->
            PartialResolution $ unlines $
            take matchTop sideA ++
            Conflict.prettyLines ((Conflict.setBodies . fmap) unmatched conflict) ++
            takeEnd matchBottom sideA
        | otherwise -> NoResolution
    where
        Sides {sideA, sideBase, sideB} = cBodies
        match base a b
            | null base = lengthOfCommonPrefix a b
            | otherwise = minimum $ map (lengthOfCommonPrefix base) [a, b]
        matchTop = match sideBase sideA sideB
        revBottom = reverse . drop matchTop
        matchBottom = match (revBottom sideBase) (revBottom sideA) (revBottom sideB)
        dropEnd count xs = take (length xs - count) xs
        takeEnd count xs = drop (length xs - count) xs
        unmatched xs = drop matchTop $ dropEnd matchBottom xs

lengthOfCommonPrefix :: Eq a => [a] -> [a] -> Int
lengthOfCommonPrefix x y = length $ takeWhile id $ zipWith (==) x y

data Result = Result
    { _resolvedSuccessfully :: !Int
    , _reducedConflicts :: !Int
    , _failedToResolve :: !Int
    , _modifiedConflicts :: !Int
    }

fullySuccessful :: Result -> Bool
fullySuccessful (Result _ reduced failed modified) = reduced == 0 && failed == 0 && modified == 0

instance Semigroup Result where
    Result x0 y0 z0 m0 <> Result x1 y1 z1 m1 = Result (x0+x1) (y0+y1) (z0+z1) (m0+m1)

instance Monoid Result where mempty = Result 0 0 0 0

data NewContent = NewContent
    { _result :: !Result
    , _newContent :: !String
    }

instance Semigroup NewContent where
    NewContent x0 y0 <> NewContent x1 y1 = NewContent (x0<>x1) (y0<>y1)

instance Monoid NewContent where mempty = NewContent mempty mempty

setResult :: (Result -> Result) -> NewContent -> NewContent
setResult f c = c {_result = f (_result c) }

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
untabify = Conflict.setBodies . fmap . fmap . untabifyStr

data LineEnding = LF | CRLF | Mixed
    deriving (Eq, Ord)

lineEnding :: String -> LineEnding
lineEnding x@(_:_) | last x == '\r' = CRLF
lineEnding _ = LF

lineEndings :: [String] -> LineEnding
lineEndings [] = Mixed
lineEndings xs =
    foldl1 f (map lineEnding xs)
    where
        f Mixed _ = Mixed
        f x c
            | x == c = x
            | otherwise = Mixed

allSame :: Eq a => [a] -> Bool
allSame (x:y:rest) = x == y && allSame (y:rest)
allSame _ = True

lineBreakFix :: Conflict -> Conflict
lineBreakFix c@Conflict{cBodies}
    | any null (toList cBodies)
    || allSame (toList endings) = c
    | otherwise =
        case resolveGen endings of
        Just LF -> (Conflict.setBodies . fmap . fmap) removeCr c
        Just CRLF -> (Conflict.setBodies . fmap . fmap) makeCr c
        _ -> c
    where
        endings = fmap lineEndings cBodies
        removeCr x@(_:_) | last x == '\r' = init x
        removeCr x = x
        makeCr x@(_:_) | last x == '\r' = x
        makeCr x = x <> "\r"

growConflict :: [Either String Conflict] -> Conflict -> Conflict
growConflict content conflict@Conflict{cMarkers} =
    foldr f
    conflict
    { cBodies = mempty
    , cMarkers = cMarkers
        { sideB = (markerBLine, takeWhile (== '=') markerBContent <> " " <> show (Conflict.Grow 0))
        }
    } content
    where
        (markerBLine, markerBContent) = sideB cMarkers
        f (Left x) c = Conflict.setBodies (fmap (x :)) c
        f (Right x) c = Conflict.setBodies (cBodies x <>) c

applyTactics :: [Either String Conflict] -> [Either String Conflict] -> (Result, [Either String Conflict])
applyTactics revDone [] = (mempty, reverse revDone)
applyTactics revDone (Left x : xs) = applyTactics (Left x : revDone) xs
applyTactics revDone (Right conflict : xs) =
    case tactic of
    Nothing -> skip
    Just (Conflict.Grow p)
        | p == 0 -> skip
        | p < 0 ->
            let (absorb, rest) = splitAt (negate p) revDone in
            first (Result 0 0 0 1 <>) $
            applyTactics (Right (growConflict (reverse absorb <> [Right conflict]) conflict) : rest) xs
        | otherwise ->
            let (absorb, rest) = splitAt p xs in
            first (Result 0 0 0 1 <>) $
            applyTactics (Right (growConflict (Right conflict : absorb) conflict) : revDone) rest
    where
        tactic = Conflict.getTactic conflict
        skip = applyTactics (Right conflict : revDone) xs

resolveContent :: Untabify -> [Either String Conflict] -> NewContent
resolveContent (Untabify mUntabifySize) raw =
    setResult (tacticsResult <>) $ foldMap go afterTactics
    where
        (tacticsResult, afterTactics) = applyTactics [] raw
        untabified = maybe id untabify mUntabifySize
        go (Left line) = NewContent mempty (unlines [line])
        go (Right conflict) =
            case (resolveConflict . lineBreakFix . untabified) conflict of
            NoResolution               -> NewContent (Result 0 0 1 0)
                                          (Conflict.pretty (untabified conflict))
            Resolution trivialLines    -> NewContent (Result 1 0 0 0) trivialLines
            PartialResolution newLines -> NewContent (Result 0 1 0 0) newLines

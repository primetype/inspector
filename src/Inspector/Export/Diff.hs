module Inspector.Export.Diff
    ( run
    ) where

import Foundation
import Foundation.VFS.FilePath (FilePath, filePathToString)
import Foundation.Collection (filter)
import Basement.Bounded (zn64)
import qualified Basement.Terminal.ANSI as ANSI
import Foundation.Monad

import Control.Monad (forM_, unless)

import Inspector.Monad (GoldenT)

import Inspector.TestVector.TestVector (TestVector, Entry(..))
import Inspector.TestVector.Value (Value)
import Inspector.TestVector.Types (Type)
import Inspector.TestVector.Key (keyToString)

run :: FilePath -> [(Word, TestVector (Type, Value, Value))] -> GoldenT ()
run path tvs = do
    -- 1. check the results of the testvectors
    let faillures = filter isInvalid tvs
    let successes = filter isValid   tvs

    -- 2. print the summary
    runTestSummary path faillures successes

    -- 3. display fails
    forM_ faillures displayFaillure

runTestSummary :: FilePath -> [(Word, TestVector (Type, Value, Value))] -> [(Word, TestVector (Type, Value, Value))] -> GoldenT ()
runTestSummary path faillures successes = do
    let numFails = fromCount $ length faillures
    let numSuccs = fromCount $ length successes
    let totals   = numFails + numSuccs

    liftIO $ putStr $ if null faillures
        then green <> " ✓ " <> reset
        else red <> " ✗ " <> reset

    liftIO $ putStrLn $ filePathToString path
                     <> ": "
                     <> show numSuccs <> "/" <> show totals

displayFaillure :: (Word, TestVector (Type, Value, Value)) -> GoldenT ()
displayFaillure (w, tv) = liftIO $ do
    putStrLn $ darkGrey <> "   >>> TestVector " <> ligthGrey <> show w <> reset
    forM_ (snd <$> toList tv) $ \ent -> do
        let key = keyToString $ entryKey ent
        let (_, v1, v2) = entryExtra ent
        unless (v1 == v2) $ do
            putStrLn $ red   <> "   - " <> key <> " = " <> show v1 <> reset
            putStrLn $ green <> "   + " <> key <> " = " <> show v2 <> reset

reset, green, red, darkGrey, ligthGrey :: ANSI.Escape
reset = ANSI.sgrReset
green = ANSI.sgrForeground (zn64 2) True
red = ANSI.sgrForeground (zn64 1) True
ligthGrey = ANSI.sgrForeground (zn64 6) True
darkGrey = ANSI.sgrForeground (zn64 6) False

isInvalid :: (Word, TestVector (Type, Value, Value)) -> Bool
isInvalid (_, tv) = or $ (entryInvalid . snd) <$> toList tv
  where
    entryInvalid ent = v1 /= v2
      where
        (_, v1, v2) = entryExtra ent

isValid :: (Word, TestVector (Type, Value, Value)) -> Bool
isValid (_, tv) = and $ (entryValid . snd) <$> toList tv
  where
    entryValid ent = v1 == v2
      where
        (_, v1, v2) = entryExtra ent
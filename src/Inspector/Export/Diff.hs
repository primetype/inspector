module Inspector.Export.Diff
    ( run
    ) where

import Foundation
import Foundation.VFS.FilePath (FilePath, filePathToString)
import Basement.Bounded (zn64)
import qualified Basement.Terminal.ANSI as ANSI
import Foundation.Monad

import Control.Monad (forM_, unless)

import Inspector.Monad (GoldenT, goldenTFail)

import Inspector.Builder (runBuilder)
import Inspector.TestVector.TestVector (TestVector, Entry(..), inputs, outputs)
import Inspector.TestVector.Value (Value, valueBuilder)
import Inspector.TestVector.Types (Type)
import Inspector.TestVector.Key (keyToString)

run :: FilePath -> [(Word, TestVector (Type, Value, Value))] -> GoldenT ()
run path tvs = do
    -- 1. check the results of the testvectors
    let warningsAndFailures  = filter isWarningOrError tvs
    let faillures = filter isInvalid tvs
    let successes = filter isValid   tvs

    unless (null faillures) goldenTFail

    -- 2. print the summary
    runTestSummary path faillures successes

    -- 3. display fails
    forM_ warningsAndFailures displayFaillure

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
    forM_ (inputs tv) $ report green orange "Warning: input may not be the same or may not be serialised the same way."
    forM_ (outputs tv) $ report green red "Error"
  where
    report ok ko msg ent = do
        let key = keyToString $ entryKey ent
        let (t, v1, v2) = entryExtra ent
        unless (v1 == v2) $ do
            let str1 = runBuilder $ valueBuilder v1 t
            let str2 = runBuilder $ valueBuilder v2 t
            putStrLn $ ko <> "   >>> " <> msg <> reset
            putStrLn $ ko <> "   - " <> key <> " = " <> str1 <> reset
            putStrLn $ ok <> "   + " <> key <> " = " <> str2 <> reset

reset, green, red, orange, darkGrey, ligthGrey :: ANSI.Escape
reset = ANSI.sgrReset
green = ANSI.sgrForeground (zn64 2) True
orange = ANSI.sgrForeground (zn64 3) True
red = ANSI.sgrForeground (zn64 1) True
ligthGrey = ANSI.sgrForeground (zn64 6) True
darkGrey = ANSI.sgrForeground (zn64 6) False

isWarningOrError :: (Word, TestVector (Type, Value, Value)) -> Bool
isWarningOrError (_, tv) = or $ entryInvalid <$> (inputs tv <> outputs tv)
  where
    entryInvalid ent = v1 /= v2
      where
        (_, v1, v2) = entryExtra ent

isInvalid :: (Word, TestVector (Type, Value, Value)) -> Bool
isInvalid (_, tv) = or $ entryInvalid <$> outputs tv
  where
    entryInvalid ent = v1 /= v2
      where
        (_, v1, v2) = entryExtra ent

isValid :: (Word, TestVector (Type, Value, Value)) -> Bool
isValid (_, tv) = and $ entryValid <$> outputs tv
  where
    entryValid ent = v1 == v2
      where
        (_, v1, v2) = entryExtra ent
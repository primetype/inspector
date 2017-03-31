module Inspector.Report
    ( TestReport (..)
    , Report (..)
    , pretty
    ) where

import Foundation
import Foundation.Monad
import Foundation.VFS
import Inspector.Dict
import Control.Monad (forM_, unless)

import System.Exit

data TestReport
    = Success | Failure Word [Diff]
  deriving (Show)

data Report = Report
    { reportPath :: !FilePath
    , reportTests :: ![TestReport]
    }
  deriving (Show)

successes :: [TestReport] -> Word
successes = foldr count 0
  where
    count Success = (+) 1
    count _       = id

failures :: [TestReport] -> [(Word, [Diff])]
failures = foldr f []
  where
    f Success       = id
    f (Failure x d) = (:) (x,d)

pretty :: MonadIO io => Report -> io ()
pretty (Report path tests) = liftIO $ do
    putStrLn $ "GoldenTest: " <> filePathToString path
    putStrLn $ "  Passed: " <> show (successes tests)
    putStrLn $ "  Failed: " <> show (toInteger $ length failed)
    putStrLn   ""
    forM_ failed $ \(i,diffs) -> do
        putStrLn $ "  * TestVector " <> show i
        forM_ diffs $ \x -> case x of
            Missing k v -> putStrLn $ "- " <> k <> " = " <> v
            Added   k v -> putStrLn $ "+ " <> k <> " = " <> v
            Diff    k (v1, v2) -> do
                           putStrLn $ "- " <> k <> " = " <> v1
                           putStrLn $ "+ " <> k <> " = " <> v2
        putStrLn ""
    unless (null failed) $ exitFailure
  where
    failed = failures tests

module Inspector.Report
    ( TestReport (..)
    , Report (..)
    , pretty
    , prettyC
    ) where

import Foundation
import Foundation.Monad
import Foundation.Conduit
import Foundation.VFS
import Inspector.Dict
import Control.Monad (forM_, unless)

import           Basement.Bounded
import qualified Basement.Terminal.ANSI as ANSI

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

total :: [TestReport] -> Word
total = foldr (\_ acc -> acc + 1) 0

prettyC :: Monad m => Conduit Report String m ()
prettyC = awaitForever $ \(Report path tests) -> do
    let failed = failures tests
    yield $ if null failed
        then green <> " ✓ " <> reset
        else red <> " ✗ " <> reset
    yield $  filePathToString path <> ": "
          <> show (successes tests) <> "/" <> show (total tests) <> "\n"

    forM_ (failures tests) $ \(i,diffs) -> do
        yield $ "  * TestVector " <> show i <> "\n"
        forM_ diffs $ \x -> case x of
            Missing k v -> yield $ "- " <> k <> " = " <> v <> "\n"
            Added   k v -> yield $ "+ " <> k <> " = " <> v <> "\n"
            Diff    k (v1, v2) -> yields
                           [ "- " <> k <> " = " <> v1 <> "\n"
                           , "+ " <> k <> " = " <> v2 <> "\n"
                           ]

reset, green, red :: ANSI.Escape
reset = ANSI.sgrReset
green = ANSI.sgrForeground (zn64 2) True
red = ANSI.sgrForeground (zn64 1) True

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

module Inspector
    ( module X
    , Config(..)
    , Mode(..)
    , runGolden
    , defaultMain
    , GoldenMT
    ) where

import Control.Monad (void)
import Foundation
import Inspector.Method  as X
import Inspector.Monad

-- | Inspector's default main function
--
-- will get the arguments and configure the 'Mode' from the command line
defaultMain :: GoldenT () -> IO ()
defaultMain suites = do
    args <- getArgs
    let mode = case args of
            []       -> GoldenTest
            ["test"] -> GoldenTest
            ["generate"] -> Generate TestVector
            ["generate", "vectors"] -> Generate TestVector
            ["generate", "rust"] -> Generate Rust
            ["generate", "c"] -> Generate C
            ["generate", "markdown"] -> Generate Markdown
            ["generate", "js"] -> Generate JS
            ["generate", "haskell"] -> Generate Haskell
            _ -> error "possible options are: <test|generate [vectors|markdown|rust|C|js|haskell]>"
    void $ runGolden (Config mode "tests/goldens") suites

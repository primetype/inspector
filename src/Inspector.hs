module Inspector
    ( -- * Golden Test
      Golden
    , golden
    , group
    , describe

    , -- ** defining a golden test
      Payload
    , (:>)
    , PathParameter

    , -- * Misc
      Config(..)
    , Mode(..)
    , defaultMain
    , GoldenMT
    , GoldenT
    ) where

import Inspector.Dict
import Inspector.Parser
import Inspector.Display
import Inspector.Monad
import Inspector.Report
import Inspector.Method

import Foundation
import Foundation.VFS.FilePath

import Control.Monad (void, forM)
import Data.List (zip3)

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
    void $ runGolden' (Config mode "tests/goldens") suites

-- | group a set of golden tests
group :: GoldenT () -> GoldenT ()
group = void . exec

-- | generate the golden test from the specification and the method
--
-- @
-- import Crypto.Hash (hash, Digest, SHA1)
--
-- type GoldenSHA1 = "hash" :> "SHA1" :> Payload "payload" String :> Payload "hash" (Digest SHA1)
-- golden (Proxy @GoldenSHA1) hash
-- @
--
golden :: Golden method
       => Proxy method
       -> Method method
       -> GoldenT ()
golden proxy action = do
    file <- mkPath path
    -- 1. collect the Dicts
    dics <- collectDics file
    -- 2. execute the method according to the plan
    r <- zip3 [1..] dics <$> forM dics (exec . method proxy action)

    mode <- getMode <$> ask
    case mode of
        GoldenTest -> do
            let rs = flip fmap r $ \(idx, org, new) ->
                        let diffs = diff org new
                         in if null diffs then Success else Failure idx diffs
            pretty $ Report path rs
        Generate TestVector -> storeBackDics file $ (\(_,_,d) -> d) <$> r
        Generate exportType -> void $ withState $ export proxy exportType (\(_,_,d) -> d <$> r)
  where
    path :: FilePath
    path = unsafeFilePath Relative (getPath proxy)

    export = undefined

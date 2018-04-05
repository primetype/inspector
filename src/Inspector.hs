{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Inspector
    ( -- * Golden Test
      Golden
    , golden
    , group
    , summary

    , -- ** defining a golden test
      Payload
    , (:>)
    , PathParameter

    , -- ** Extending Inspectable
      Inspectable(..)
    , reportError
    , OutputType(..)
    , withBoolean
    , withInteger
    , withDouble
    , withString
    , withCollection
    , withStructure
    , field

    , -- * Misc
      Config(..)
    , Mode(..)
    , defaultMain
    , defaultTest
    , GoldenMT
    , GoldenT
    ) where

import Inspector.Monad
import Inspector.Method
import Inspector.Export.Types
import Inspector.Export.RefFile
import qualified Inspector.Export.RefFile as RefFile
import qualified Inspector.Export.Diff as Diff
import qualified Inspector.Export.Rust as Rust
import qualified Inspector.Export.Markdown as Markdown

import           Inspector.TestVector.Types      (Type)
import           Inspector.TestVector.Value      (Value)
import           Inspector.TestVector.TestVector (Entry(..), TestVector)

import Foundation
import Foundation.Monad
import Foundation.Conduit
import Foundation.VFS.FilePath

import GHC.TypeLits (KnownSymbol)

import Control.Monad (void, when)
import Data.Version (Version(..))

import qualified Console.Options as CLI

-- | handy one for test suite for cabal
defaultTest :: GoldenT () -> IO ()
defaultTest = CLI.defaultMain . runCommandTest

-- | Inspector's default main function
--
-- will get the arguments and configure the 'Mode' from the command line
defaultMain :: GoldenT () -> IO ()
defaultMain suites = CLI.defaultMain $ do
    CLI.programName "inspector"
    CLI.programVersion $ Version [0,1] ["alpha"]
    CLI.programDescription "Golden Tests and test vectors management"

    CLI.command "test" $ runCommandTest suites
    CLI.command "generate" $ runCommandGenerate suites

runCommandTest :: GoldenT () -> CLI.OptionDesc (IO ()) ()
runCommandTest suites = do
    goldenpath <- CLI.flagParam (CLI.FlagShort 'd' <> CLI.FlagLong "root" <> CLI.FlagDescription "root path for the golden tests")
                                (CLI.FlagRequired (Right . fromString))
    CLI.action $ \get -> do
        let p = fromMaybe "tests/goldens" (get goldenpath)
        void $ runGolden' (Config GoldenTest p False) $ do
            void $ suites
            t <- goldenTFailed
            when t $ error "Failed due to previous errors."

runCommandGenerate :: GoldenT () -> CLI.OptionDesc (IO ()) ()
runCommandGenerate suites = do
    CLI.command "rust" $ do
        CLI.description "Generate the test vector in rust compatible format."
        generate (Generate Rust)
    CLI.command "markdown" $ do
        CLI.description "Generate markdown output of the test vectors."
        generate (Generate Markdown)
    CLI.command "test-vectors" $ do
        CLI.description "Generate the test vector output"
        generate (Generate TestVectors)
  where
    generate gen = do
        goldenpath <- CLI.flagParam (CLI.FlagShort 'd' <> CLI.FlagLong "root" <> CLI.FlagDescription "root path for the golden tests")
                                    (CLI.FlagRequired (Right . fromString))
        out <- CLI.flag $ CLI.FlagLong "stdout" <> CLI.FlagDescription "generate to stdout instead of the appropriate file path"

        CLI.action $ \get -> do
            let p = fromMaybe "tests/goldens" (get goldenpath)
            void $ runGolden' (Config gen p (get out)) $ do
                void $ suites
                t <- goldenTFailed
                when t $ error "Failed due to previous errors."

-- | group a set of golden tests
group :: GoldenT () -> GoldenT ()
group = id -- void . exec

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
    mode <- getMode <$> ask

    file <- mkPath input
    -- 1. collect the testvectors
    !tv1 <- liftIO $ toList <$> parseTestVectorFile file
    -- 2. run the method against each TestVector
    !tv2 <- runConduit $ yields tv1 .| traverseWith store proxy action .| sinkList
    -- 3. keep only result
    let tv3 = flip fmap tv2 $ \(a,_,c) -> (a, c)
    case mode of
        GoldenTest        -> Diff.run input tv3
        Generate Rust     -> Rust.run input tv3
        Generate Markdown -> Markdown.run input tv3
        Generate TestVectors -> RefFile.run input tv3
  where
    input :: FilePath
    input = unsafeFilePath Relative path'

    path' = getPath proxy

traverseWith :: forall method c . (Golden method, Monoid c)
             => (forall a k . (IsValue a, KnownSymbol k) => Proxy k -> Entry (Type, Value, a) -> GoldenMT c IO ())
             -> Proxy method
             -> Method method
             -> Conduit (TestVector ()) (Word, TestVector (), c) GoldenT ()
traverseWith f proxy action = awaitIndex $ \idx dict -> do
    c <- lift $ exec $ method proxy action f dict
    yield (idx, dict, c)

awaitIndex :: (Word -> input -> Conduit input output m b) -> Conduit input output m ()
awaitIndex f = go 1
  where
    go acc = do
        mv <- await
        case mv of
            Nothing -> pure ()
            Just v  -> f acc v >> go (succ acc)

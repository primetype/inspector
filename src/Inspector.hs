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
    , OutputType(..)

    , -- * Misc
      Config(..)
    , Mode(..)
    , defaultMain
    , GoldenMT
    , GoldenT
    ) where

import Inspector.Monad
import Inspector.Method
import Inspector.Export.Types
import Inspector.Export.RefFile
import qualified Inspector.Export.Diff as Diff

import           Inspector.TestVector.Types      (Type)
import           Inspector.TestVector.Value      (Value)
import           Inspector.TestVector.TestVector (Entry(..), TestVector)

import Foundation
import Foundation.Monad
import Foundation.Conduit
import Foundation.VFS.FilePath

import GHC.TypeLits (KnownSymbol)

import Control.Monad (void)


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
            ["generate", "markdown"] -> Generate Markdown
            _ -> error "possible options are: <test|generate [vectors|markdown|rust]>"
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
    mode <- getMode <$> ask

    file <- mkPath input
    -- 1. collect the testvectors
    !tv1 <- liftIO $ toList <$> parseTestVectorFile file
    -- 2. run the method against each TestVector
    !tv2 <- runConduit $ yields tv1 .| traverseWith store proxy action .| sinkList

    case mode of
        GoldenTest -> Diff.run input (fmap (\(w, _, tv) -> (w, tv)) tv2)
        _          -> undefined -- TODO

    {-
    let c = case mode of
                GoldenTest -> traverseWith store proxy action
                           .| diffC
                           .| (sinkList >>= (yield . Report input))
                           .| prettyC
                _ -> undefined
    output' <- maybe (pure Nothing) (fmap Just . mkPath) (output mode)
    (close, h) <- liftIO $ case output' of
        Nothing -> pure (\_ -> pure (), stdout)
        Just p  -> (closeFile,) <$> openFile p WriteMode
    runConduit $  yields dics .| c .| toBytes UTF8 .| sinkHandle h
    liftIO $ close h
-}
  where
    input :: FilePath
    input = unsafeFilePath Relative path'

    path' = getPath proxy
{-
    output :: Mode -> Maybe FilePath
    output (Generate TestVector) = Nothing
    output (Generate Markdown) = Nothing -- Just $ fromString (filePathToLString input <> ".md")
    output (Generate Rust) = Nothing -- Just $  fromString (filePathToLString input <> ".rs")
    output GoldenTest = Nothing
-}
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
    
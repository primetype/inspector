{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Inspector.Dict
import Inspector.Monad
import Inspector.Report
import Inspector.Method
import Inspector.Export.Types hiding (input, output)
import Inspector.Export.RefFile

import qualified Inspector.Export.Markdown as Markdown
import qualified Inspector.Export.Rust as Rust

import Foundation
import Foundation.Monad
import Foundation.IO (stdout, openFile, closeFile, IOMode(WriteMode))
import Foundation.String (Encoding(UTF8))
import Foundation.Conduit
import Foundation.Conduit.Textual
import Foundation.VFS.FilePath


import GHC.TypeLits

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
    -- 1. collect the Dicts
    dics <- liftIO $ toList <$> parseRefFile file
    -- 2. execute the method according to the plan
    let c = case mode of
                GoldenTest -> traverseWith store proxy action
                           .| diffC
                           .| (sinkList >>= (yield . Report input))
                           .| prettyC
                Generate TestVector -> traverseWith store proxy action
                                    .| genC
                                    .| storeBackDictC
                Generate Markdown -> do
                    Markdown.summary proxy
                    traverseWith store proxy action .| genC .| Markdown.pop proxy
                Generate Rust -> do
                    Rust.summary proxy
                    yield $  "const GOLDEN_TESTS : [TestVector;"<> show (fromCount (length dics)) <>"] =\n"
                    traverseWith store proxy action .| genC .| Rust.pop proxy
                    yield "  ];\n\n"
    output' <- maybe (pure Nothing) (fmap Just . mkPath) (output mode)
    (close, h) <- liftIO $ case output' of
        Nothing -> pure (\_ -> pure (), stdout)
        Just p  -> (closeFile,) <$> openFile p WriteMode
    runConduit $  yields dics .| c .| toBytes UTF8 .| sinkHandle h
    liftIO $ close h
  where
    input :: FilePath
    input = unsafeFilePath Relative path'

    path' = getPath proxy

    output :: Mode -> Maybe FilePath
    output (Generate TestVector) = Just input
    output (Generate Markdown) = Just $ fromString (filePathToLString input <> ".md")
    output (Generate Rust) = Just $  fromString (filePathToLString input <> ".rs")
    output GoldenTest = Nothing

awaitIndex :: (Word -> input -> Conduit input output m b) -> Conduit input output m ()
awaitIndex f = go 1
  where
    go acc = do
        mv <- await
        case mv of
            Nothing -> pure ()
            Just v  -> f acc v >> go (succ acc)

diffC :: Monad m => Conduit (Word, Dict, Dict) TestReport m ()
diffC = awaitForever $ \(idx, old, new) ->
    let diffs = diff old new
     in yield $ if null diffs then Success else Failure idx diffs

genC :: Monad m => Conduit (Word, Dict, a) (Word, a) m ()
genC = awaitForever $ \(idx, _, new) -> yield (idx, new)

traverseWith :: forall method c . (Golden method, Monoid c)
             => (forall a k . (Value a, KnownSymbol k) => Proxy k -> a -> GoldenMT c IO ())
             -> Proxy method
             -> Method method
             -> Conduit Dict (Word, Dict, c) GoldenT ()
traverseWith f proxy action = awaitIndex $ \idx dict -> do
    c <- lift $ exec $ method proxy action f dict
    yield (idx, dict, c)

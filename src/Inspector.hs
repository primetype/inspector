{-# LANGUAGE Rank2Types #-}

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
import Inspector.Export.Types

import qualified Inspector.Export.Markdown as Markdown

import Foundation
import Foundation.IO (stdout)
import Foundation.String (Encoding(UTF8))
import Foundation.Conduit
import Foundation.Conduit.Textual
import Foundation.VFS.FilePath

import Basement.Nat
import GHC.TypeLits

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

    file <- mkPath path
    -- 1. collect the Dicts
    dics <- collectDics file
    -- 2. execute the method according to the plan
    let c = case mode of
                GoldenTest -> traverseWith store proxy action
                           .| diffC
                           .| (sinkList >>= (yield . Report path))
                           .| prettyC
                Generate TestVector -> traverseWith store proxy action
                                    .| genC
                                    .| storeBackC
                Generate Markdown -> do
                    Markdown.summary proxy
                    traverseWith store proxy action .| genC .| Markdown.pop proxy
    runConduit $  yields dics .| c .| toBytes UTF8 .| sinkHandle stdout
  where
    path :: FilePath
    path = unsafeFilePath Relative (getPath proxy)

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
             => (forall a k m . (Value a, KnownSymbol k) => Proxy k -> a -> GoldenMT c IO ())
             -> Proxy method
             -> Method method
             -> Conduit Dict (Word, Dict, c) GoldenT ()
traverseWith f proxy action = awaitIndex $ \idx dict -> do
    c <- lift $ exec $ method proxy action f dict
    yield (idx, dict, c)

{-
    r <- zip3 [1..] dics <$> forM dics (exec . method proxy action)
    case mode of
        GoldenTest -> do
            let rs = flip fmap r $ \(idx, org, new) ->
                        let diffs = diff org new
                         in if null diffs then Success else Failure idx diffs
            pretty $ Report path rs
        Generate TestVector -> storeBackDics file $ (\(_,_,d) -> d) <$> r
        Generate t -> void $ do
            st <- withState $ \st -> (st, st)
            export t proxy file st $ (\(_,_,d) -> d) <$> r
-}

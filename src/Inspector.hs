{-# LANGUAGE Rank2Types #-}
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

import Basement.Terminal as Terminal (initialize)

import Foundation
import Foundation.Monad
import Foundation.Conduit
import Foundation.VFS.FilePath

import GHC.TypeLits (KnownSymbol)

import Control.Monad (void, when)
import System.Console.GetOpt
import qualified System.Environment as S (getArgs)

-- | handy one for test suite for cabal
defaultTest :: GoldenT () -> IO ()
defaultTest suites = do
    Terminal.initialize
    args <- S.getArgs
    (opts, command) <- case getOpt Permute options args of
        (o, cmd,  []) -> return (o, cmd)
        (_,   _, err) -> error $ show err
    case command of
        [] -> runCommandTest opts suites
        ["help"] -> usage
        ["test"] -> runCommandTest opts suites
        ["generate"] -> runCommandGenerate opts "test-vectors" suites
        ["generate", target] -> runCommandGenerate opts target suites
        _ -> usage >> error ("invalid commands: " <> show command)
  where
    usage = putStrLn "\
\usage: inspector [OPTIONS] [COMMANDS]\n\
\\n\
\OPTIONS:\n\
\  `-d|--root <DIR>' the route directory where the golden tests are (default `./tests/goldens')\n\
\  `--stdout'        display the generated output to stdout instead of files\n\
\\n\
\COMMANDS:\n\
\  `help'              displays this help message\n\
\  `test'              runs the golden tests suite (default command if none given)\n\
\  `generate [TARGET]' generates the outputs for different test suites : `rust' or `markdown'\n\
\\n\
\"

data InspectorOption
    = RootDir LString
    | GenStdout
  deriving (Show, Eq, Ord, Typeable)

options :: [OptDescr InspectorOption]
options =
    [ Option ['d'] ["root"] (ReqArg RootDir "DIR") "root directory of the golden test path"
    , Option [] ["stdout"] (NoArg GenStdout) "generate to stdout"
    ]

getRootDir :: [InspectorOption] -> FilePath
getRootDir [] = "tests/goldens"
getRootDir (RootDir dir:_) = fromString (fromList dir)
getRootDir (_: xs) = getRootDir xs

getStdoutOpt :: [InspectorOption] -> Bool
getStdoutOpt [] = False
getStdoutOpt (GenStdout : _) = True
getStdoutOpt (_:xs) = getStdoutOpt xs

runCommandTest :: [InspectorOption] -> GoldenT () -> IO ()
runCommandTest o suites = void $ runGolden' (Config GoldenTest (getRootDir o) (getStdoutOpt o)) $ do
    void $ suites
    t <- goldenTFailed
    when t $ error "Failed due to previous errors."

runCommandGenerate :: [InspectorOption] -> LString -> GoldenT () -> IO ()
runCommandGenerate o target suites = case target of
    "test-vectors" -> generate (Generate TestVectors)
    "rust" -> generate (Generate Rust)
    "markdown" -> generate (Generate Markdown)
    _ -> error $ "unknown target: " <> show target
  where
    generate gen = void $ runGolden' (Config gen (getRootDir o) (getStdoutOpt o)) $ do
        void suites
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

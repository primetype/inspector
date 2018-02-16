{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Inspector.Monad
    ( GoldenMT
    , Config(..)
    , Mode(..)
    , OutputType(..)
    , runGolden
    , runGolden'
    , ask
    , lift
    , withState
    , exec
    , mkPath

    , GoldenM
    , store

    , GoldenT

    , describe
    , Builder
    ) where

import Foundation
import Foundation.Monad
import Foundation.Monad.Reader
import Foundation.Monad.State
import Foundation.VFS
import Foundation.String.Builder

import GHC.TypeLits

import Inspector.Display
import Inspector.Dict
import Inspector.Report

data OutputType = TestVector | Markdown | Rust | C | JS | Haskell
  deriving (Show, Eq, Ord, Enum, Bounded, Typeable)

data Mode = Generate !OutputType
          | GoldenTest
  deriving (Show, Eq, Ord, Typeable)

data Config = Config
    { getMode :: !Mode
    , getRoot :: !FilePath
    }
  deriving (Show, Eq, Typeable)

newtype GoldenMT st m a = GoldenM { runGoldenM_ :: StateT st (ReaderT Config m) a }
  deriving (Typeable, Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadIO)
instance MonadTrans (GoldenMT st) where
    lift = GoldenM . lift . lift
instance Monad m => MonadState (GoldenMT st m) where
    type State (GoldenMT st m) = st
    withState = GoldenM . withState
instance Monad m => MonadReader (GoldenMT st m) where
    type ReaderContext (GoldenMT st m) = Config
    ask = GoldenM $ lift ask

runGolden :: Functor m => Config -> st ->  GoldenMT st m a -> m (a, st)
runGolden cfg st golden = runReaderT (runStateT (runGoldenM_ golden) st) cfg

runGolden' :: (Monoid st, Functor m) => Config -> GoldenMT st m a -> m (a, st)
runGolden' cfg = runGolden cfg mempty

exec :: (Monoid st, Monad m) => GoldenMT st m a -> GoldenMT st' m st
exec golden = do
    cfg <- ask
    snd <$> lift (runGolden' cfg golden)

mkPath :: Monad m => FilePath -> GoldenMT st m FilePath
mkPath target = do
    (p, root, _) <- splitPath . getRoot <$> ask
    let (_, t, _) = splitPath target
    pure $ buildPath (p, root <> t, ())

-- | Monad responsible for controlling the execution flow of the test vectors
--
newtype Metadata = Metadata
    { metaDescription :: Builder
    }
  deriving (Typeable, Semigroup, Monoid)

type GoldenT = GoldenMT Metadata IO

describe :: Builder -> GoldenT ()
describe builder = withState $ \st -> ((), st { metaDescription = builder })

-- | Monad for a running golden test
--
type GoldenM = GoldenMT Dict IO

store :: (KnownSymbol key, Display value)
      => Proxy (key :: Symbol) -> value -> GoldenM ()
store pk val = withState $ \dict ->
    ((), add pk (display val) dict )

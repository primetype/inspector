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
    , Metadata(..)
    , summary
    , getMetadata
    , goldenTFail
    , goldenTFailed
    , Builder
    ) where

import Foundation hiding ((<>))
import Foundation.Monad
import Foundation.Monad.Reader
import Foundation.Monad.State
import Foundation.VFS
import Foundation.String.Builder

import Basement.Compat.Semigroup

import GHC.TypeLits

import Inspector.Export.Types
import Inspector.TestVector.TestVector (TestVector(..), Entry(..), add)
import Inspector.TestVector.Types (Type)
import Inspector.TestVector.Value (Value)

data Mode = Generate !OutputType
          | GoldenTest
  deriving (Show, Eq, Ord, Typeable)

data Config = Config
    { getMode :: !Mode
    , getRoot :: !FilePath
    , getStdout :: !Bool
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

data Metadata = Metadata
    { metaDescription  :: !String
    , goldenTestFailed :: !Bool
    }
  deriving (Show, Eq, Typeable)
instance Semigroup Metadata where
    (<>) = mappend
instance Monoid Metadata where
    mempty = Metadata mempty False
    mappend (Metadata d1 t1) (Metadata d2 t2) = Metadata (d1 <> d2) (t1 && t2)

-- | Monad responsible for controlling the execution flow of the test vectors
--
type GoldenT = GoldenMT Metadata IO

summary :: String -> GoldenT ()
summary b = withState $ \st -> ((), st { metaDescription = b })

goldenTFail :: GoldenT ()
goldenTFail = withState $ \st -> ((), st {goldenTestFailed = True})

goldenTFailed :: GoldenT Bool
goldenTFailed = goldenTestFailed <$> getMetadata

getMetadata :: GoldenT Metadata
getMetadata = withState $ \st -> (st, st)

-- | Monad for a running golden test
--
type GoldenM = GoldenMT (TestVector (Type, Value, Value)) IO

store :: (KnownSymbol key, Inspectable value)
      => Proxy (key :: Symbol) -> Entry (Type, Value, value) -> GoldenM ()
store pk ent = withState $ \dict ->
    ((), add pk ent' dict)
  where
    (t, _, a) = entryExtra ent
    ent' = Entry
        { entryType  = entryType ent
        , entryValue = entryValue ent
        , entryKey   = entryKey ent
        , entryInput = entryInput ent
        , entryDoc   = entryDoc ent
        , entryExtra = (t, entryValue ent, builder a)
        }

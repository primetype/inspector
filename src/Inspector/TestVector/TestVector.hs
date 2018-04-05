{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Inspector.TestVector.TestVector
    ( TestVector(..)
    , testVectorParser
    , testVectorBuilder
    , Entry(..)
    , entryParser
    , entryBuilder
    , query
    , add
    , add'
    , inputs
    , outputs
    ) where

import           Foundation hiding (String, Array)
import           Foundation.Collection (KeyedCollection, Element, IndexedCollection(..))
import qualified Foundation            as F (String)
import qualified Foundation.Collection as F

import           Control.Monad (void, forM_)
import           GHC.TypeLits

import           Inspector.Parser (Parser)
import qualified Inspector.Parser as Parser
import           Inspector.Builder
import           Inspector.TestVector.Types (Type)
import           Inspector.TestVector.Value (Value, valueParser, getValueType, valueBuilder)
import           Inspector.TestVector.Key   (Key, keyParser, symbolKey_, keyToString)

data Entry a = Entry
    { entryKey   :: !Key
    , entryValue :: !Value
    , entryType  :: !Type
    , entryInput :: !(Maybe Bool)
    , entryDoc   :: !(Maybe F.String)
    , entryExtra :: !a
    }
  deriving (Show, Eq, Ord, Typeable)

entryParser :: Parser (Entry ())
entryParser = do
    key <- keyParser
    Parser.whiteSpaces >> Parser.element '=' >> Parser.whiteSpaces
    value <- valueParser
    pure $ Entry key value (getValueType value) Nothing Nothing ()

entryBuilder :: Entry (Type, Value) -> Builder ()
entryBuilder Entry{..} = do
    emit (keyToString entryKey) >> emit " = "
    -- align to the given key length
    indent $ length (keyToString entryKey) + 3
    -- build the value based on the type
    valueBuilder (snd entryExtra) (fst entryExtra)
    -- remove the alignment
    unindent
    newline

newtype TestVector a = TestVector [(Key, Entry a)]
  deriving (Show, Eq, Ord, Typeable, Semigroup, Monoid, Collection, Sequential, IndexedCollection, Foldable)

type instance Element (TestVector a) = (Key, Entry a)

instance KeyedCollection (TestVector a) where
    type Key (TestVector a)   = Key
    type Value (TestVector a) = Entry a
    lookup k = F.lookup k . toList

instance IsList (TestVector a) where
    type Item (TestVector a) = (Key, Entry a)
    toList (TestVector l) = l
    fromList = TestVector

inputs :: TestVector a -> [Entry a]
inputs = filter (fromMaybe undefined . entryInput) . fmap snd . toList

outputs :: TestVector a -> [Entry a]
outputs = filter (not . fromMaybe undefined . entryInput) . fmap snd . toList

query :: KnownSymbol key => Proxy key -> TestVector a -> Maybe (Entry a)
query = F.lookup . symbolKey_

add' :: Key -> Entry a -> TestVector a -> TestVector a
add' key val (TestVector l) = TestVector $ (key, val) : l

add :: KnownSymbol key => Proxy key -> Entry a -> TestVector a -> TestVector a
add p = add' (symbolKey_ p)

testVectorParser :: Parser (TestVector ())
testVectorParser = fmap fromList $ do
    void $ Parser.many Parser.comment
    Parser.elements "TestVector"
    Parser.newline
    Parser.some go
  where
    go = do
        Parser.whiteSpacesAndNewLines
        r <- entryParser <* Parser.newline
        Parser.whiteSpacesAndNewLines
        pure (entryKey r, r)

testVectorBuilder :: TestVector (Type, Value) -> Builder ()
testVectorBuilder tvs = do
    emit "TestVector" >> newline
    forM_ (toList tvs) (entryBuilder . snd)
    newline

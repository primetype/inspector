{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inspector.Dict
    ( Dict
    , dictToList
    , lookup
    , query
    , add
    , Diff(..)
    , diff

    , storeBackC
    ) where

import Foundation hiding (takeWhile)
import Foundation.Conduit
import Foundation.Collection hiding (takeWhile)
import Data.List (deleteFirstsBy)

import GHC.TypeLits

newtype Dict = Dict { dictToList :: [(String, IntermediarType)] }
  deriving (Show, Eq, Ord, Typeable, Semigroup, Monoid, Collection, Sequential, IndexedCollection, Foldable)
instance KeyedCollection Dict where
    type Key Dict = String
    type Value Dict = IntermediarType
    lookup k = lookup k . dictToList

type instance Element Dict = (String, IntermediarType)
instance IsList Dict where
    type Item Dict = (String, IntermediarType)
    fromList = Dict
    toList   = dictToList

data Diff = Missing String IntermediarType
          | Diff String (IntermediarType, IntermediarType)
          | Added String IntermediarType
  deriving (Show)

diff :: Dict -> Dict -> [Diff]
diff (Dict d1) (Dict d2) = left <> right <> both
  where
    left  = uncurry Missing <$> deleteFirstsBy comp d1 d2
    right = uncurry Added <$> deleteFirstsBy comp d2 d1
    both  = [ Diff k (x, y) | (k,x) <- d1, (k2,y) <- d2, k == k2, x /= y ]
    comp (a, _) (b, _) = a == b

query :: KnownSymbol key => Proxy (key :: Symbol) -> Dict -> Maybe IntermediarType
query p = lookup key
  where
    key = fromList (symbolVal p)

add' :: String -> IntermediarType -> Dict -> Dict
add' key val (Dict l) = Dict $ (key, val) : l

add :: KnownSymbol key => Proxy (key :: Symbol) -> IntermediarType -> Dict -> Dict
add p = add' key
  where
    key = fromList (symbolVal p)

storeBackC :: Monad m => Conduit (Word, Dict) String m ()
storeBackC = awaitForever $ \(i, l) -> do
    yield $ "# Test Vector " <> show i <> "\n"
    yield "TestVector\n"
    forM_ (sortBy f $ toList l) $ \(k, _) ->
      yield k >> yield " = " >> undefined >> yield "\n"
    yield "\n"
  where
    f (x,_) (y,_) = compare x y

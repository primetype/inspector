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

    , collectDics
    , storeBackC
    ) where

import Foundation hiding (takeWhile, skipWhile)
import Foundation.Parser
import Foundation.Monad
import Foundation.String (Encoding (..))
import Foundation.Conduit
import Foundation.Conduit.Textual
import Foundation.IO
import Foundation.VFS
import Foundation.Collection hiding (takeWhile, skipWhile)
import Data.ByteArray (Bytes)
import Data.List (deleteFirstsBy, intersectBy, zip)

import GHC.TypeLits

newtype Dict = Dict { dictToList :: [(String, String)] }
  deriving (Show, Eq, Ord, Typeable, Semigroup, Monoid, Collection, Sequential, IndexedCollection, Foldable)
instance KeyedCollection Dict where
    type Key Dict = String
    type Value Dict = String
    lookup k = lookup k . dictToList

type instance Element Dict = (String, String)
instance IsList Dict where
    type Item Dict = (String, String)
    fromList = Dict
    toList   = dictToList

data Diff = Missing String String
          | Diff String (String, String)
          | Added String String
  deriving (Show)

diff :: Dict -> Dict -> [Diff]
diff (Dict d1) (Dict d2) = left <> right <> both
  where
    left  = uncurry Missing <$> deleteFirstsBy comp d1 d2
    right = uncurry Added <$> deleteFirstsBy comp d2 d1
    both  = [ Diff k (x, y) | (k,x) <- d1, (k2,y) <- d2, k == k2, x /= y ]
    comp (a, _) (b, _) = a == b

query :: KnownSymbol key => Proxy (key :: Symbol) -> Dict -> Maybe String
query p = lookup key
  where
    key = fromList (symbolVal p)

add' :: String -> String -> Dict -> Dict
add' key val (Dict l) = Dict $ (key, val) : l

add :: KnownSymbol key => Proxy (key :: Symbol) -> String -> Dict -> Dict
add p = add' key
  where
    key = fromList (symbolVal p)

collectDics :: MonadIO io => FilePath -> io [Dict]
collectDics path = liftIO $ withFile path ReadMode $ \h -> runConduit $
    sourceHandle h .| fromBytes UTF8
                   .| lines
                   .| stripComment
                   .| dict
                   .| sinkList
  where
    stripComment :: Monad m => Conduit String String m ()
    stripComment = awaitForever f
      where
        f str
          | null str = pure ()
          | str ! 0 == Just '#' = pure ()
          | otherwise = yield str
    dict :: Monad m => Conduit String Dict m ()
    dict = do
        ms <- await
        case ms of
            Nothing -> pure () -- we are done
            Just "TestVector" -> go mempty
            Just ""           -> dict
            Just _            -> error "Expecting the String 'TestVector'"
    go :: Monad m => Dict -> Conduit String Dict m ()
    go acc = do
        ms <- await
        case ms of
            Nothing -> yield acc
            Just "TestVector" -> yield acc >> go mempty
            Just ""           -> go acc
            Just str          ->
                case parseOnly parseKeyVal str of
                    Left err -> error (show (str, err))
                    Right (k,v) -> go (add' k v acc)
    parseKeyVal :: Parser String (String, String)
    parseKeyVal = do
        key <- takeWhile (not . flip elem [' ', '\t', '='])
        ignoreWhiteSpace
        element '='
        ignoreWhiteSpace
        val <- takeAll
        pure (key, val)
      where
        ignoreWhiteSpace :: Parser String ()
        ignoreWhiteSpace = skipWhile (`elem` [' ', '\t'])

storeBackC :: Monad m => Conduit (Word, Dict) String m ()
storeBackC = awaitForever $ \(i, l) -> do
    yield $ "# Test Vector " <> show i <> "\n"
    yield "TestVector\n"
    forM_ (sortBy f $ toList l) $ \(k, v) ->
      yield k >> yield " = " >> yield v >> yield "\n"
    yield "\n"
  where
    f (x,_) (y,_) = compare x y

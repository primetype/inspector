{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}

module Inspector.Method
    ( (:>)
    , Payload
    , HasPath
    , getPath
    , HasMethod
    , Method
    , method
    , PathParameter
    , IsValue
    , Golden
    ) where

import Foundation
import Foundation.VFS.FilePath
import Foundation.Check

import Control.Monad (void)
import GHC.TypeLits
import Data.Typeable

import Inspector.Monad
import Inspector.Export.Types
import           Inspector.TestVector.Key (symbolKey_)
import           Inspector.TestVector.Types (Type)
import           Inspector.TestVector.Value (Value)
import           Inspector.TestVector.TestVector (TestVector, query, Entry(..))


-- | Alias Constraint Type for IsValue type
type IsValue value = (Inspectable value, Typeable value)

-- | Alias Constraint type for golden test
type Golden golden = (HasMethod golden, HasPath golden)


data (i :: k) :> o
  deriving (Typeable)
infixr 9 :>

-- | Type level alias to describe what to retrive from the test vector
--
-- a Test vector can be as follow:
--
-- @
-- TestVector
-- key = value
-- @
--
data Payload  (key :: Symbol) value
  deriving (Typeable)

data PathParameter (key :: Symbol) (n :: Nat)

-- | type class to retrieve the Path of a given Golden Spec
--
-- @
-- import Crypto.Hash (hash, Digest, SHA1)
--
-- type GoldenSHA1 = "hash" :> "SHA1" :> Payload "payload" String :> Payload "hash" (Digest SHA1)
-- getPath (Proxy @GoldenSHA1) = ["hash", "SHA1"]
-- @
--
-- This will help us know where to find the test vectors
--
-- The path discoverability in the type stop at the first occurence of a 'Payload'
--
class HasPath path where
    getPath :: Proxy path -> [FileName]

instance {-# OVERLAPPABLE #-} (KnownSymbol path, HasPath sub) => HasPath (path :> sub) where
    getPath _ = fromString (symbolVal (Proxy @path)) : getPath (Proxy @sub)
instance {-# OVERLAPPABLE #-} (KnownNat n, KnownSymbol path, HasPath sub) => HasPath (PathParameter path n :> sub) where
    getPath _ = fromString (toList $ path <> n) : getPath (Proxy @sub)
      where
        n    = show       (natVal    (Proxy @n))
        path = fromString (symbolVal (Proxy @path))
instance {-# OVERLAPPING #-} KnownSymbol path => HasPath (path :> Payload k v :> sub) where
    getPath _ = [fromString (symbolVal (Proxy @path))]
instance {-# OVERLAPPING #-} (KnownNat n, KnownSymbol path) => HasPath (PathParameter path n :> Payload k v :> sub) where
    getPath _ = [fromString (toList $ path <> n)]
      where
        n    = show       (natVal    (Proxy @n))
        path = fromString (symbolVal (Proxy @path))

-- | Type class to retrieve the parameter of the given method and to call the
-- method on the fly
--
class HasMethod method where
    type Method method

    method :: forall c b m . Monad m
           => Proxy method
           -> Method method
           -> (forall a k . (IsValue a, KnownSymbol k) => Proxy k -> Entry (Type, Value, a) -> GoldenMT c m b)
           -> TestVector ()
           -> GoldenMT c m b

instance (KnownSymbol path, HasMethod sub) => HasMethod (path :> sub) where
    type Method (path :> sub) = Method sub
    method _ = method (Proxy @sub)

instance (KnownSymbol path, KnownNat n, HasMethod sub) => HasMethod (PathParameter path n :> sub) where
    type Method (PathParameter path n :> sub) = Method sub
    method _ = method (Proxy @sub)

instance (KnownSymbol key, HasMethod sub, IsValue value, Arbitrary value) => HasMethod (Payload key value :> sub) where
    type Method (Payload key value :> sub) = value -> Method sub

    method _ action f dict = do
        mvalue <- retrieve @key @value Proxy dict
        value <- case mvalue of
            Nothing -> error $ "missing key: " <> fromString (symbolVal (Proxy @key))
            Just value -> pure value
        void $ f (Proxy @key) value
        let (_, _, v) = entryExtra value
        method (Proxy @sub) (action v) f dict

instance (KnownSymbol key, IsValue value) => HasMethod (Payload key value) where
    type Method (Payload key value) = value

    method _ action f dict = do
        ma <- retrieve @key @value Proxy dict
        f (Proxy @key) (finalEntry (Proxy @key) ma action)

finalEntry :: forall a k . (Inspectable a, KnownSymbol k)
           => Proxy k -> Maybe (Entry (Type, Value, a)) -> a -> Entry (Type, Value, a)
finalEntry p Nothing a =
    let e = createEntry (symbolKey_ p) a
     in e { entryExtra = (entryType e, entryValue e, a) }
finalEntry _ (Just e) a =
    let (t, v, _) = entryExtra e
     in e { entryExtra = (t, v, a)}

instance ( KnownSymbol k1, IsValue v1
         , KnownSymbol k2, IsValue v2
         )
      => HasMethod ( Payload k1 v1
                   , Payload k2 v2
                   )
   where
    type Method ( Payload k1 v1
                , Payload k2 v2
                ) = (v1, v2)

    method _ action f dict = do
        mv1 <- retrieve @k1 @v1 Proxy dict
        mv2 <- retrieve @k2 @v2 Proxy dict
        let (v1, v2) = action
        let e1 = finalEntry (Proxy @k1) mv1 v1
        let e2 = finalEntry (Proxy @k2) mv2 v2
        void $ f (Proxy @k1) e1
        f (Proxy @k2) e2

instance ( KnownSymbol k1, IsValue v1
         , KnownSymbol k2, IsValue v2
         , KnownSymbol k3, IsValue v3
         )
      => HasMethod ( Payload k1 v1
                   , Payload k2 v2
                   , Payload k3 v3
                   )
   where
    type Method ( Payload k1 v1
                , Payload k2 v2
                , Payload k3 v3
                ) = (v1, v2, v3)

    method _ action f dict = do
        mv1 <- retrieve @k1 @v1 Proxy dict
        mv2 <- retrieve @k2 @v2 Proxy dict
        mv3 <- retrieve @k3 @v3 Proxy dict
        let (v1, v2, v3) = action
        let e1 = finalEntry (Proxy @k1) mv1 v1
        let e2 = finalEntry (Proxy @k2) mv2 v2
        let e3 = finalEntry (Proxy @k3) mv3 v3
        void $ f (Proxy @k1) e1
        void $ f (Proxy @k2) e2
        f (Proxy @k3) e3

instance ( KnownSymbol k1, IsValue v1
         , KnownSymbol k2, IsValue v2
         , KnownSymbol k3, IsValue v3
         , KnownSymbol k4, IsValue v4
         )
      => HasMethod ( Payload k1 v1
                   , Payload k2 v2
                   , Payload k3 v3
                   , Payload k4 v4
                   )
   where
    type Method ( Payload k1 v1
                , Payload k2 v2
                , Payload k3 v3
                , Payload k4 v4
                ) = (v1, v2, v3, v4)

    method _ action f dict = do
        mv1 <- retrieve @k1 @v1 Proxy dict
        mv2 <- retrieve @k2 @v2 Proxy dict
        mv3 <- retrieve @k3 @v3 Proxy dict
        mv4 <- retrieve @k4 @v4 Proxy dict
        let (v1, v2, v3, v4) = action
        let e1 = finalEntry (Proxy @k1) mv1 v1
        let e2 = finalEntry (Proxy @k2) mv2 v2
        let e3 = finalEntry (Proxy @k3) mv3 v3
        let e4 = finalEntry (Proxy @k4) mv4 v4
        void $ f (Proxy @k1) e1
        void $ f (Proxy @k2) e2
        void $ f (Proxy @k3) e3
        f (Proxy @k4) e4

instance ( KnownSymbol k1, IsValue v1
         , KnownSymbol k2, IsValue v2
         , KnownSymbol k3, IsValue v3
         , KnownSymbol k4, IsValue v4
         , KnownSymbol k5, IsValue v5
         )
      => HasMethod ( Payload k1 v1
                   , Payload k2 v2
                   , Payload k3 v3
                   , Payload k4 v4
                   , Payload k5 v5
                   )
   where
    type Method ( Payload k1 v1
                , Payload k2 v2
                , Payload k3 v3
                , Payload k4 v4
                , Payload k5 v5
                ) = (v1, v2, v3, v4, v5)

    method _ action f dict = do
        mv1 <- retrieve @k1 @v1 Proxy dict
        mv2 <- retrieve @k2 @v2 Proxy dict
        mv3 <- retrieve @k3 @v3 Proxy dict
        mv4 <- retrieve @k4 @v4 Proxy dict
        mv5 <- retrieve @k5 @v5 Proxy dict
        let (v1, v2, v3, v4, v5) = action
        let e1 = finalEntry (Proxy @k1) mv1 v1
        let e2 = finalEntry (Proxy @k2) mv2 v2
        let e3 = finalEntry (Proxy @k3) mv3 v3
        let e4 = finalEntry (Proxy @k4) mv4 v4
        let e5 = finalEntry (Proxy @k5) mv5 v5
        void $ f (Proxy @k1) e1
        void $ f (Proxy @k2) e2
        void $ f (Proxy @k3) e3
        void $ f (Proxy @k4) e4
        f (Proxy @k5) e5

instance ( KnownSymbol k1, IsValue v1
         , KnownSymbol k2, IsValue v2
         , KnownSymbol k3, IsValue v3
         , KnownSymbol k4, IsValue v4
         , KnownSymbol k5, IsValue v5
         , KnownSymbol k6, IsValue v6
         )
      => HasMethod ( Payload k1 v1
                   , Payload k2 v2
                   , Payload k3 v3
                   , Payload k4 v4
                   , Payload k5 v5
                   , Payload k6 v6
                   )
   where
    type Method ( Payload k1 v1
                , Payload k2 v2
                , Payload k3 v3
                , Payload k4 v4
                , Payload k5 v5
                , Payload k6 v6
                ) = (v1, v2, v3, v4, v5, v6)

    method _ action f dict = do
        mv1 <- retrieve @k1 @v1 Proxy dict
        mv2 <- retrieve @k2 @v2 Proxy dict
        mv3 <- retrieve @k3 @v3 Proxy dict
        mv4 <- retrieve @k4 @v4 Proxy dict
        mv5 <- retrieve @k5 @v5 Proxy dict
        mv6 <- retrieve @k6 @v6 Proxy dict
        let (v1, v2, v3, v4, v5, v6) = action
        let e1 = finalEntry (Proxy @k1) mv1 v1
        let e2 = finalEntry (Proxy @k2) mv2 v2
        let e3 = finalEntry (Proxy @k3) mv3 v3
        let e4 = finalEntry (Proxy @k4) mv4 v4
        let e5 = finalEntry (Proxy @k5) mv5 v5
        let e6 = finalEntry (Proxy @k6) mv6 v6
        void $ f (Proxy @k1) e1
        void $ f (Proxy @k2) e2
        void $ f (Proxy @k3) e3
        void $ f (Proxy @k4) e4
        void $ f (Proxy @k5) e5
        f (Proxy @k6) e6

-- helper method to retrieve a value from a dictionary
retrieve :: forall key value c m
          . ( IsValue value
            , Monad m
            , KnownSymbol key
            )
         => Proxy (key :: Symbol)
         -> TestVector ()
         -> GoldenMT c m (Maybe (Entry (Type, Value, value)))
retrieve pk dict = case query pk dict of
    Nothing -> pure Nothing
    Just e ->
        let en = checkEntryType (Proxy @value) e
         in case parser @value (snd $ entryExtra en) of
            Left err -> error $ err <> "\n  While decoding " <> k <> " of type " <> show ty
            Right v  -> pure $ Just $ Entry
                { entryKey = entryKey en
                , entryType = entryType en
                , entryValue = entryValue en
                , entryExtra = (fst (entryExtra en), snd (entryExtra en), v)
                }
  where
    k  = show $ symbolVal pk
    ty = show $ typeRep (Proxy @value)

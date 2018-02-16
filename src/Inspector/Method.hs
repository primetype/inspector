{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Inspector.Method
    ( (:>)
    , Payload
    , HasPath
    , getPath
    , HasMethod
    , Method
    , method
    , PathParameter
    , Value
    , Golden
    ) where

import Foundation
import Foundation.VFS.FilePath
import Foundation.Check
import Foundation.Monad
import Foundation.Monad.Reader
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.ByteArray (ByteArray, Bytes)
import Data.ByteArray.Encoding

import Control.Monad (when, void, forM, forM_)
import GHC.TypeLits
import Data.Typeable

import Inspector.Dict
import Inspector.Parser
import Inspector.Display
import Inspector.Monad
import Inspector.Report

-- | Alias Constraint Type for Value type
type Value value = (HasParser value, Typeable value, Display value)

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
-- This is used when generating or testing the test vectors.
--
-- @
-- import Crypto.Hash (hash, Digest, SHA1)
--
-- type GoldenSHA1 = "hash" :> "SHA1" :> Payload "payload" String :> Payload "hash" (Digest SHA1)
--
-- test :: (String -> Digest SHA1) -> Dict -> GoldenM ()
-- test = method (Proxy @GoldenSHA1)
-- @
--
class HasMethod method where
    type Method method

    method :: Proxy method -> Method method -> Dict -> GoldenM ()

instance (KnownSymbol path, HasMethod sub) => HasMethod (path :> sub) where
    type Method (path :> sub) = Method sub
    method _ = method (Proxy @sub)

instance (KnownSymbol path, KnownNat n, HasMethod sub) => HasMethod (PathParameter path n :> sub) where
    type Method (PathParameter path n :> sub) = Method sub
    method _ = method (Proxy @sub)

instance (KnownSymbol key, HasMethod sub, Value value, Arbitrary value) => HasMethod (Payload key value :> sub) where
    type Method (Payload key value :> sub) = value -> Method sub

    method _ action dict = do
        mvalue <- retrieve @key @value Proxy dict
        value <- case mvalue of
            Nothing -> error $ "missing key: " <> fromString (symbolVal (Proxy @key))
            Just value -> pure value
        store (Proxy @key) value
        method (Proxy @sub) (action value) dict

instance (KnownSymbol key, Value value) => HasMethod (Payload key value) where
    type Method (Payload key value) = value

    method methProxy action dict = do
        void $ retrieve @key @value Proxy dict
        store (Proxy @key) action

instance ( KnownSymbol k1, Value v1
         , KnownSymbol k2, Value v2
         )
      => HasMethod ( Payload k1 v1
                   , Payload k2 v2
                   )
   where
    type Method ( Payload k1 v1
                , Payload k2 v2
                ) = (v1, v2)

    method methProxy action dict = do
        void $ retrieve @k1 @v1 Proxy dict
        void $ retrieve @k2 @v2 Proxy dict
        let (v1, v2) = action
        store (Proxy @k1) v1
        store (Proxy @k2) v2

instance ( KnownSymbol k1, Value v1
         , KnownSymbol k2, Value v2
         , KnownSymbol k3, Value v3
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

    method methProxy action dict = do
        void $ retrieve @k1 @v1 Proxy dict
        void $ retrieve @k2 @v2 Proxy dict
        void $ retrieve @k3 @v3 Proxy dict
        let (v1, v2, v3) = action
        store (Proxy @k1) v1
        store (Proxy @k2) v2
        store (Proxy @k3) v3

instance ( KnownSymbol k1, Value v1
         , KnownSymbol k2, Value v2
         , KnownSymbol k3, Value v3
         , KnownSymbol k4, Value v4
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

    method methProxy action dict = do
        void $ retrieve @k1 @v1 Proxy dict
        void $ retrieve @k2 @v2 Proxy dict
        void $ retrieve @k3 @v3 Proxy dict
        void $ retrieve @k4 @v4 Proxy dict
        let (v1, v2, v3, v4) = action
        store (Proxy @k1) v1
        store (Proxy @k2) v2
        store (Proxy @k3) v3
        store (Proxy @k4) v4

-- helper method to retrieve a value from a dictionary
retrieve :: forall key value
          . ( Value value
            , KnownSymbol key
            )
         => Proxy (key :: Symbol)
         -> Dict
         -> GoldenM (Maybe value)
retrieve pk dict = case query (Proxy @key) dict of
    Nothing -> pure Nothing
    Just st -> case parseOnly getParser st of
        Left (Expected w g) -> error $ show ("parse (" <> r <> " :: " <> t <> ")", st, "Expected: " <> w <> "; But received: " <> g)
        Left (ExpectedElement w g) -> error $ show ("parse (" <> r <> " :: " <> t <> ")", st, "Expected: " <> show w <> "; But received: " <> show g)
        Left err            -> error $ show ("parse (" <> r <> " :: " <> t <> ")", st, err)
        Right r  -> pure (Just r)
  where
    r = fromList $ symbolVal (Proxy @key)
    t = show $ typeRep (Proxy @value)
